/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2018, VU University Amsterdam
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

:- module(swish_version,
          [ check_prolog_version/1,     % +NumericVersion
            prolog_version_atom/1,      % -Atom
            register_git_module/2,      % +Name, +Options
            git_module_property/2,      % ?Name, ?Property
            git_update_versions/1       % ?Name
          ]).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(git)).


/** <module> Manage software versions

The module deals with software  versions.   It  currently implements two
features:  test  whether   SWI-Prolog   is    sufficiently   new   using
check_prolog_version/1 and find GIT version   signatures for the running
server. Modules that want  their  version   info  available  through the
web-page can do so using a call to register_git_module/2.
*/

:- multifile
    git_module_hook/3.              % Name, Dir, Options

%!  check_prolog_version(+Required) is det.
%
%   Validate the program is running under Prolog version Required or
%   newer. Required is in numeric notation (e.g. 70718 for 7.7.18)
%
%   @error prolog_version_error(Required) if Prolog does not have
%   the required version.

check_prolog_version(Required) :-
    prolog_version_ok(Required),
    !.
check_prolog_version(Required) :-
    throw(error(prolog_version_error(Required), _)).

prolog_version_ok(or(V1, V2)) :-
    !,
    (   prolog_version_ok(V1)
    ->  true
    ;   prolog_version_ok(V2)
    ).
prolog_version_ok(Required) :-
    current_prolog_flag(version, MyVersion),
    MyVersion >= Required.

%!  prolog_version_atom(-Atom) is det.
%
%   Atom describes the current Prolog version

prolog_version_atom(Version) :-
    current_prolog_flag(version_git, Version),
    !.
prolog_version_atom(Version) :-
    current_prolog_flag(version_data, swi(Major,Minor,Patch,Options)),
    (   memberchk(tag(Tag), Options)
    ->  format(atom(Version), '~w.~w.~w-~w', [Major, Minor, Patch, Tag])
    ;   format(atom(Version), '~w.~w.~w',    [Major, Minor, Patch])
    ).



:- multifile
    prolog:message//1,
    prolog:error_message//1.

prolog:error_message(prolog_version_error(Required)) -->
    { current_prolog_flag(version, MyVersion),
      user_version(MyVersion, MyV),
      user_version(Required, Req)
    },
    [ 'This program requires SWI-Prolog ~w'-[Req], nl,
      'while you are running version ~w.'-[MyV], nl,
      'Please visit http://www.swi-prolog.org and', nl,
      'upgrade your version of SWI-Prolog.'
    ].
prolog:message(git(no_version)) -->
    [ 'Sorry, cannot retrieve version stamp from GIT.' ].
prolog:message(git(update_versions)) -->
    [ 'Updating GIT version stamps in the background.' ].


user_version(or(V1,V2), Version) :-
    !,
    user_version(V1, A1),
    user_version(V2, A2),
    format(atom(Version), '~w or ~w', [A1, A2]).
user_version(N, Version) :-
    Major is N // 10000,
    Minor is (N // 100) mod 100,
    Patch is N mod 100,
    atomic_list_concat([Major, Minor, Patch], '.', Version).


                 /*******************************
                 *         REGISTRATION         *
                 *******************************/

:- dynamic
    git_module/3,           % Name, Dir, Options
    git_module_prop/3.      % Name, Property, Value

%!  register_git_module(+Name, +Options)
%
%   Register the directory from which the  Prolog file was loaded as
%   a GIT component about which to  report version information. This
%   should be used as a directive.  Defined options:
%
%       * directory(Dir)
%       Use Dir as the location of the GIT repository instead of the
%       directory of the file from which this directive was called.
%       If Dir is not absolute, it is taken relative to the
%       directory holding the file from which this directive was called.
%
%       * home_url(URL)
%       Used to create a link to the components home-page.

register_git_module(Name, Options) :-
    (   prolog_load_context(directory, BaseDir)
    ->  true
    ;   working_directory(BaseDir, BaseDir)
    ),
    select_option(directory(Dir), Options, RestOptions, '.'),
    absolute_file_name(Dir, AbsDir,
                       [ file_type(directory),
                         relative_to(BaseDir),
                         access(read)
                       ]),
    retractall(git_module(Name, _, _)),
    assert(git_module(Name, AbsDir, RestOptions)).

git_update_versions(Name) :-
    catch(forall(current_git_module(Name, _, _),
                 update_version(Name)),
          _,
          print_message(warning, git(no_version))).

update_version(Name) :-
    current_git_module(Name, Dir, Options),
    (   catch(git_describe(GitVersion, [directory(Dir)|Options]), _, fail)
    ->  true
    ;   GitVersion = unknown
    ),
    retractall(git_module_prop(Name, version, _)),
    assert(git_module_prop(Name, version, GitVersion)).

current_git_module(Name, Dir, Options) :-
    git_module(Name, Dir, Options).
current_git_module(Name, Dir, Options) :-
    git_module_hook(Name, Dir, Options).


%!  git_module_property(?Name, ?Property) is nondet.
%
%   Property is a property of the named git-component. Defined
%   properties are:
%
%       - version(Version)
%         git-describe like version information
%       - directory(Dir)
%         Base directory of the component
%       - remote(URL)
%         Location we are cloned from
%       - home_url(URL)
%         Project home

git_module_property(Name, Property) :-
    (   var(Name)
    ->  current_git_module(Name, _, _),
        git_module_property(Name, Property)
    ;   compound(Property)
    ->  once(gen_module_property(Name, Property))
    ;   gen_module_property(Name, Property)
    ).

gen_module_property(Name, version(Version)) :-
    (   git_module_prop(Name, version, Version0)
    ->  true
    ;   git_update_versions(Name),
        git_module_prop(Name, version, Version0)
    ),
    Version0 \== unknown,
    Version = Version0.
gen_module_property(Name, directory(Dir)) :-
    current_git_module(Name, Dir, _).
gen_module_property(Name, remote(Alias, Remote)) :-
    (   ground(Alias)
    ->  true
    ;   Alias = origin
    ),
    current_git_module(Name, Dir, _),
    (   git_module_prop(Name, remote, Alias-Remote)
    ->  true
    ;   git_remote_url(Alias, Remote, [directory(Dir)]),
        asserta(git_module_prop(Name, remote, Alias-Remote))
    ).
gen_module_property(Name, Term) :-
    current_git_module(Name, _, Options),
    member(Term, Options).



                 /*******************************
                 *        KEEP UP-TO-DATE       *
                 *******************************/

bg_git_update_versions :-
    print_message(informational, git(update_versions)),
    thread_create(git_update_versions(_), _,
                  [ detached(true)
                  ]).

:- multifile
    user:message_hook/3.

user:message_hook(make(done(_)), _, _) :-
    bg_git_update_versions,
    fail.

% do not update versions in background because we need to fork
:- if(current_predicate(http_unix_daemon:http_daemon/0)).
:- initialization git_update_versions(_).
:- else.
:- initialization bg_git_update_versions.
:- endif.
