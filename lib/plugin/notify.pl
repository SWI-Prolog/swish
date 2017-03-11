/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2017, VU University Amsterdam
			 CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(swish_notify,
          [ follow/3,                           % +DocID, +ProfileID, +Options
            notify/2                            % +DocID, +Action
          ]).
:- use_module(library(settings)).
:- use_module(library(persistency)).
:- use_module(library(broadcast)).
:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(library(user_profile)).

:- use_module(email).
:- use_module('../bootstrap').
:- use_module('../storage').
:- use_module('../chat').

:- initialization
    start_mail_scheduler.

/** <module> SWISH notifications

This module keeps track of which users wish to track which notifications
and sending the notifications to the user.  If the target user is online
we will notify using an avatar.  Otherwise we send an email.

A user has the following options to control notifications:

  * Per (gitty) file
    - Notify update
    - Notify chat
  * By profile
    - Notify by E-mail: never/immediate/daily

@tbd    Allow user to update Profile value nicely
@tbd	Allow user to edit options for a file.  This dialog
	may also update the profile?
*/

:- setting(database, callable, swish('data/notify.db'),
           "Database holding notifications").
:- setting(queue, callable, swish('data/notify-queue.db'),
           "File holding queued messages").
:- setting(daily, compound, 04:00,
           "Time at which to send daily messages").


		 /*******************************
		 *            DATABASE		*
		 *******************************/

:- persistent
        follower(docid:atom,
                 profile:atom,
                 options:list(oneof([update,chat]))).

notify_open_db :-
    db_attached(_),
    !.
notify_open_db :-
    setting(database, Spec),
    absolute_file_name(Spec, Path, [access(write)]),
    db_attach(Path, [sync(close)]).

%!  queue_event(+Profile, +Action) is det.
%!  queue_event(+Profile, +Action, +Status) is det.
%
%   Queue an email notification for  Profile,   described  by Action. We
%   simply append these events as Prolog terms to a file.

queue_event(Profile, Action) :-
    queue_event(Profile, Action, new).
queue_event(Profile, Action, Status) :-
    queue_file(Path),
    with_mutex(swish_notify,
               queue_event_sync(Path, Profile, Action, Status)).

queue_event_sync(Path, Profile, Action, Status) :-
    setup_call_cleanup(
        open(Path, append, Out, [encoding(utf8)]),
        format(Out, '~q.~n', [notify(Profile, Action, Status)]),
        close(Out)).

queue_file(Path) :-
    setting(queue, Spec),
    absolute_file_name(Spec, Path, [access(write)]).

%!  send_queued_mails is det.
%
%   Send possible queued emails.

send_queued_mails :-
    queue_file(Path),
    exists_file(Path), !,
    atom_concat(Path, '.sending', Tmp),
    with_mutex(swish_notify, rename_file(Path, Tmp)),
    read_file_to_terms(Tmp, Terms, [encoding(utf8)]),
    forall(member(Term, Terms),
           send_queued(Term)),
    delete_file(Tmp).
send_queued_mails.

send_queued(notify(Profile, Action, Status)) :-
    profile_property(Profile, email(Email)),
    profile_property(Profile, email_notifications(When)),
    When \== never, !,
    (   catch(send_notification_mail(Profile, Email, Action),
              Error, true)
    ->  (   var(Error)
        ->  true
        ;   update_status(Status, Error, NewStatus)
        ->  queue_event(Profile, Action, NewStatus)
        ;   true
        )
    ;   update_status(Status, failed, NewStatus)
    ->  queue_event(Profile, Action, NewStatus)
    ;   true
    ).

update_status(new, Status, retry(3, Status)).
update_status(retry(Count0, _), Status, retry(Count, Status)) :-
    Count0 > 0,
    Count is Count0 - 1.

%!  start_mail_scheduler
%
%   Start a thread that schedules queued mail handling.

start_mail_scheduler :-
    catch(thread_create(mail_main, _,
                        [ alias(mail_scheduler),
                          detached(true)
                        ]),
          error(permission_error(create, thread, mail_scheduler), _),
          true).

%!  mail_main
%
%   Infinite loop that schedules sending queued messages.

mail_main :-
    repeat,
    next_send_queue_time(T),
    get_time(Now),
    Sleep is T-Now,
    sleep(Sleep),
    thread_create(send_queued_mails, _,
                  [ detached(true),
                    alias(send_queued_mails)
                  ]),
    fail.

next_send_queue_time(T) :-
    get_time(Now),
    stamp_date_time(Now, date(Y,M,D0,H0,_M,_S,Off,TZ,DST), local),
    setting(daily, HH:MM),
    (   H0 @< HH
    ->  D = D0
    ;   D is D0+1
    ),
    date_time_stamp(date(Y,M,D,HH,MM,0,Off,TZ,DST), T).


%!  follow(+DocID, +ProfileID, +Options) is det.
%
%   Assert that DocID is being followed by ProfileID using Options.

follow(DocID, ProfileID, Options) :-
    to_atom(DocID, DocIDA),
    to_atom(ProfileID, ProfileIDA),
    notify_open_db,
    (   follower(DocIDA, ProfileIDA, OldOptions)
    ->  (   OldOptions == Options
        ->  true
        ;   retractall_follower(DocIDA, ProfileIDA, _),
            assert_follower(DocIDA, ProfileIDA, Options)
        )
    ;   assert_follower(DocIDA, ProfileIDA, Options)
    ).

%!  notify(+DocID, +Action) is det.
%
%   Action has been executed on DocID.  Notify all interested users.
%   Actions that may be notified:
%
%   - updated(Commit)
%     Gitty file was updated
%   - deleted(Commit)
%     Gitty file was deleted
%   - forked(Commit)
%     Gitty file was forked
%   - chat(Message)
%     A chat message was sent.  Message is the JSON content as a dict.

notify(DocID, Action) :-
    to_atom(DocID, DocIDA),
    notify_open_db,
    forall(follower(DocIDA, Profile, Options),
           notify_user(Profile, Action, Options)).

to_atom(Text, Atom) :-
    atom_string(Atom, Text).

%!  notify_user(+Profile, +Action, +Options)
%
%   Notify the user belonging to Profile about Action.

:- meta_predicate try(0).

notify_user(Profile, Action, Options) :-
    try(notify_chat(Profile, Action, Options)),
    try(notify_by_mail(Profile, Action, Options)).

try(Goal) :-
    catch(Goal, Error, print_message(error, Error)),
    !.
try(Goal) :-
    print_message(error, goal_failed(Goal)).




		 /*******************************
		 *         BROADCAST API	*
		 *******************************/

:- unlisten(swish(_)),
   listen(swish(Event), notify_event(Event)).

% request to follow this file
notify_event(follow(DocID, ProfileID, Options)) :-
    follow(DocID, ProfileID, Options).
% events on gitty files
notify_event(updated(File, Commit)) :-
    atom_concat('gitty:', File, DocID),
    notify(DocID, updated(Commit)).
notify_event(deleted(File, Commit)) :-
    atom_concat('gitty:', File, DocID),
    notify(DocID, deleted(Commit)).
notify_event(created(_File, Commit)) :-
    storage_meta_data(Commit.get(previous), Meta),
    atom_concat('gitty:', Meta.name, DocID),
    notify(DocID, forked(Commit)).
% chat message
notify_event(chat(Message)) :-
    notify(Message.docid, chat(Message)).


		 /*******************************
		 *            CHAT		*
		 *******************************/

notify_chat(ProfileID, Action, _Options) :-
    chat_to_profile(ProfileID, \chat(Action)).

chat(updated(Commit)) -->
    html([\committer(Commit), ' updated ', \file_name(Commit)]).


		 /*******************************
		 *            EMAIL		*
		 *******************************/

%!  notify_by_mail(+Profile, +Action, +FollowOptions) is semidet.
%
%   Send a notification by mail. Optionally  schedules the message to be
%   send later.
%
%   @tbd: if sending fails, should we queue the message?

notify_by_mail(Profile, Action, Options) :-
    profile_property(Profile, email(Email)),
    profile_property(Profile, email_notifications(When)),
    When \== never,
    must_notify(Action, Options),
    (   When == immediate
    ->  debug(notify(email), 'Sending notification mail to ~p', [Profile]),
        send_notification_mail(Profile, Email, Action)
    ;   debug(notify(email), 'Queing notification mail to ~p', [Profile]),
        queue_event(Profile, Action)
    ).

must_notify(chat(_), Options) :- !,
    memberchk(chat, Options).
must_notify(_, Options) :-
    memberchk(update, Options).

%!  send_notification_mail(+Profile, +Action) is semidet.
%
%   Actually send a notification mail.  Fails   if  Profile  has no mail
%   address or does not want to be notified by email.

send_notification_mail(Profile, Email, Action) :-
    phrase(subject(Action), Codes),
    string_codes(Subject, Codes),
    smtp_send_html(Email, \message(Profile, Action),
                   [ subject(Subject)
                   ]).

subject(Action) -->
    subject_prefix,
    subject_action(Action).

subject_prefix -->
    "[SWISH] ".

subject_action(updated(Commit)) -->
    txt_commit_file(Commit), " updated by ", txt_committer(Commit).


		 /*******************************
		 *            HTML BODY		*
		 *******************************/

message(ProfileID, Action) -->
    dear(ProfileID),
    notification(Action),
    signature.

dear(ProfileID) -->
    html(h4(["Dear ", \profile_name(ProfileID), ","])), !.
dear(_) -->
    [].

profile_name(ProfileID) -->
    { profile_property(ProfileID, name(Name)) },
    html(Name).

signature -->
    { public_url(swish, [], HREF, []) },
    html(address(['SWISH at ', a(href(HREF),HREF)])).

notification(updated(Commit)) -->
    html(p(['The file ', \file_name(Commit),
            ' has been updated by ', \committer(Commit), '.'])),
    commit_message(Commit).

file_name(Commit) -->
    { public_url(web_storage, path_postfix(Commit.name), HREF, []) },
    html(a(href(HREF), Commit.name)).

committer(Commit) -->
    { ProfileID = Commit.get(profile_id) }, !,
    profile_name(ProfileID).
committer(Commit) -->
    html(Commit.get(owner)).

commit_message(Commit) -->
    { Message = Commit.get(commit_message) }, !,
    html(p(class('commit-message'), Message)).
commit_message(_Commit) -->
    html(p(class('no-commit-message'), 'No message')).



		 /*******************************
		 *  TEXT RULES ON GITTY COMMITS	*
		 *******************************/

txt_commit_file(Commit) -->
    write(Commit.name).

txt_committer(Commit) -->
    { ProfileID = Commit.get(profile_id) }, !,
    txt_profile_name(ProfileID).
txt_committer(Commit) -->
    write(Commit.get(owner)), !.



		 /*******************************
		 *    RULES ON GITTY COMMITS	*
		 *******************************/

txt_profile_name(ProfileID) -->
    { profile_property(ProfileID, name(Name)) },
    write(Name).


		 /*******************************
		 *            BASICS		*
		 *******************************/

write(Term, Head, Tail) :-
    format(codes(Head, Tail), '~w', [Term]).


		 /*******************************
		 *        HTTP HANDLING		*
		 *******************************/

:- http_handler(swish(follow/options), follow_file_options,
                [ id(follow_file_options) ]).
:- http_handler(swish(follow/save), save_follow_file,
                [ id(save_follow_file) ]).

%!  follow_file_options(+Request)
%
%   Edit the file following options for the current user.

follow_file_options(Request) :-
    http_parameters(Request,
                    [ docid(DocID, [atom])
                    ]),
    http_in_session(_SessionID),
    http_session_data(profile_id(ProfileID)),
    profile_property(ProfileID, email_notifications(When)),

    (   follower(DocID, ProfileID, Follow)
    ->  true
    ;   Follow = []
    ),

    follow_file_widgets(When, Follow, Widgets),

    reply_html_page(
        title('Follow file options'),
        \bt_form(Widgets,
                 [ class('form-horizontal'),
                   label_columns(sm-3)
                 ])).

:- multifile
    user_profile:attribute/3.

follow_file_widgets(When, Follow,
    [ select(follow, [update,chat], [value(Follow), multiple(true)]),
      select(email_notifications, NotificationOptions, [value(When)])
    | Buttons
    ]) :-
    user_profile:attribute(email_notifications, oneof(NotificationOptions), _),
    buttons(Buttons).

buttons(
    [ button_group(
          [ button(save, submit,
                   [ type(primary),
                     data([action(SaveHREF)])
                   ]),
            button(cancel, button,
                   [ type(danger),
                     data([dismiss(modal)])
                   ])
          ],
          [
          ])
    ]) :-
    http_link_to_id(save_follow_file, [], SaveHREF).

%!  save_follow_file(+Request)
%
%   Save the follow file options

save_follow_file(Request) :-
    http_read_json_dict(Request, Dict),
    debug(profile(update), 'Got ~p', [Dict]),
    http_in_session(_SessionID),
    http_session_data(profile_id(_ProfileID)),
    debug(notify(options), 'Set follow options to ~p', [Dict]),
    reply_json_dict(_{status:success}).
