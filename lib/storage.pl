/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2016, VU University Amsterdam
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

:- module(web_storage,
	  [ storage_file/1,			% ?File
	    storage_file/3			% +File, -Data, -Meta
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/mimetype)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(solution_sequences)).

:- use_module(page).
:- use_module(gitty).
:- use_module(patch).
:- use_module(config).
:- use_module(search).

/** <module> Store files on behalve of web clients

The file store needs to deal  with   versioning  and  meta-data. This is
achieved using gitty.pl, a git-like content-base  store that lacks git's
notion of a _tree_. I.e., all files   are considered individual and have
their own version.
*/

:- setting(directory, atom, storage, 'The directory for storing files.').

:- http_handler(swish('p/'), web_storage, [ id(web_storage), prefix ]).

:- initialization open_gittystore.

open_gittystore :-
	setting(directory, Dir),
	(   exists_directory(Dir)
	->  true
	;   make_directory(Dir)
	),
	gitty_open(Dir, []).


%%	web_storage(+Request) is det.
%
%	Restfull HTTP handler to store data on behalf of the client in a
%	hard-to-guess location. Returns a JSON  object that provides the
%	URL for the data and the plain   file name. Understands the HTTP
%	methods =GET=, =POST=, =PUT= and =DELETE=.

web_storage(Request) :-
	option(method(Method), Request),
	storage(Method, Request).

:- multifile
	swish_config:authenticate/2.

storage(get, Request) :-
	(   swish_config:authenticate(Request, User)
	->  Options = [user(User)]
	;   Options = []
	),
	http_parameters(Request,
			[ format(Fmt,  [ oneof([swish,raw,json,history,diff]),
					 default(swish),
					 description('How to render')
				       ]),
			  depth(Depth, [ default(5),
					 integer,
					 description('History depth')
				       ]),
			  to(RelTo,    [ optional(true),
					 description('Diff relative to')
				       ])
			]),
	(   Fmt == history
	->  (   nonvar(RelTo)
	    ->	Format = history(Depth, RelTo)
	    ;	Format = history(Depth)
	    )
	;   Fmt == diff
	->  Format = diff(RelTo)
	;   Format = Fmt
	),
	storage_get(Request, Format, Options).

storage(post, Request) :-
	http_read_json_dict(Request, Dict),
	option(data(Data), Dict, ""),
	option(type(Type), Dict, pl),
	setting(directory, Dir),
	make_directory_path(Dir),
	meta_data(Request, Dir, Dict, Meta),
	(   atom_string(Base, Dict.get(meta).get(name))
	->  file_name_extension(Base, Type, File),
	    (	catch(gitty_create(Dir, File, Data, Meta, Commit),
		      error(gitty(file_exists(File)),_),
		      fail)
	    ->	true
	    ;	Error = json{error:file_exists,
			     file:File}
	    )
	;   (   repeat,
	        random_filename(Base),
		file_name_extension(Base, Type, File),
		catch(gitty_create(Dir, File, Data, Meta, Commit),
		      error(gitty(file_exists(File)),_),
		      fail)
	    ->  true
	    )
	),
	(   var(Error)
	->  debug(storage, 'Created: ~p', [Commit]),
	    storage_url(File, URL),

	    reply_json_dict(json{url:URL,
				 file:File,
				 meta:Commit.put(symbolic, "HEAD")
				})
	;   reply_json_dict(Error)
	).
storage(put, Request) :-
	http_read_json_dict(Request, Dict),
	setting(directory, Dir),
	request_file(Request, Dir, File),
	(   Dict.get(update) == "meta-data"
	->  gitty_data(Dir, File, Data, _OldMeta)
	;   option(data(Data), Dict, "")
	),
	meta_data(Request, Dir, Dict, Meta),
	storage_url(File, URL),
	catch(gitty_update(Dir, File, Data, Meta, Commit),
	      Error,
	      true),
	(   var(Error)
	->  debug(storage, 'Updated: ~p', [Commit]),
	    reply_json_dict(json{url:URL,
				 file:File,
				 meta:Commit.put(symbolic, "HEAD")
			    })
	;   update_error(Error, Dir, Data, File, URL)
	).
storage(delete, Request) :-
	authentity(Request, Meta),
	setting(directory, Dir),
	request_file(Request, Dir, File),
	gitty_update(Dir, File, "", Meta, _New),
	reply_json_dict(true).

%%	update_error(+Error, +Storage, +Data, +File, +URL)
%
%	If error signals an edit conflict, prepare an HTTP =|409
%	Conflict|= page

update_error(error(gitty(commit_version(_, Head, Previous)), _),
	     Dir, Data, File, URL) :- !,
	gitty_diff(Dir, Previous, Head, OtherEdit),
	gitty_diff(Dir, Previous, data(Data), MyEdits),
	Status0 = json{url:URL,
		       file:File,
		       error:edit_conflict,
		       edit:_{server:OtherEdit,
			      me:MyEdits}
		      },
	(   OtherDiff = OtherEdit.get(data)
	->  PatchOptions = [status(_), stderr(_)],
	    patch(Data, OtherDiff, Merged, PatchOptions),
	    Status1 = Status0.put(merged, Merged),
	    foldl(patch_status, PatchOptions, Status1, Status)
	;   Status = Status0
	),
	reply_json_dict(Status, [ status(409) ]).
update_error(Error, _Dir, _Data, _File, _URL) :-
	throw(Error).

patch_status(status(exit(0)), Dict, Dict) :- !.
patch_status(status(exit(Status)), Dict, Dict.put(patch_status, Status)) :- !.
patch_status(status(killed(Signal)), Dict, Dict.put(patch_killed, Signal)) :- !.
patch_status(stderr(""), Dict, Dict) :- !.
patch_status(stderr(Errors), Dict, Dict.put(patch_errors, Errors)) :- !.


request_file(Request, Dir, File) :-
	option(path_info(File), Request),
	(   gitty_file(Dir, File, _Hash)
	->  true
	;   http_404([], Request)
	).

storage_url(File, HREF) :-
	http_link_to_id(web_storage, path_postfix(File), HREF).

%%	meta_data(+Request, +Dict, -Meta) is det.
%%	meta_data(+Request, +Store, +Dict, -Meta) is det.
%
%	Gather meta-data from the  Request   (user,  peer)  and provided
%	meta-data. Illegal and unknown values are ignored.

meta_data(Request, Dict, Meta) :-
	authentity(Request, Meta0),	% user, peer
	(   filter_meta(Dict.get(meta), Meta1)
	->  Meta = Meta0.put(Meta1)
	;   Meta = Meta0
	).

meta_data(Request, Store, Dict, Meta) :-
	meta_data(Request, Dict, Meta1),
	(   atom_string(Previous, Dict.get(previous)),
	    is_sha1(Previous),
	    gitty_commit(Store, Previous, _PrevMeta)
	->  Meta = Meta1.put(previous, Previous)
	;   Meta = Meta1
	).

filter_meta(Dict0, Dict) :-
	dict_pairs(Dict0, Tag, Pairs0),
	filter_pairs(Pairs0, Pairs),
	dict_pairs(Dict, Tag, Pairs).

filter_pairs([], []).
filter_pairs([H|T0], [H|T]) :-
	H = K-V,
	meta_allowed(K, Type),
	is_of_type(Type, V), !,
	filter_pairs(T0, T).
filter_pairs([_|T0], T) :-
	filter_pairs(T0, T).

meta_allowed(public,	     boolean).
meta_allowed(example,	     boolean).
meta_allowed(author,	     string).
meta_allowed(email,	     string).
meta_allowed(title,	     string).
meta_allowed(tags,	     list(string)).
meta_allowed(description,    string).
meta_allowed(commit_message, string).

%%	storage_get(+Request, +Format, +Options) is det.
%
%	HTTP handler that returns information a given gitty file.
%
%	@arg Format is one of
%
%	     - swish
%	     Serve file embedded in a SWISH application
%	     - raw
%	     Serve the raw file
%	     - json
%	     Return a JSON object with the keys `data` and `meta`
%	     - history(Depth, IncludeHASH)
%	     Return a JSON description with the change log
%	     - diff(RelTo)
%	     Reply with diff relative to RelTo.  Default is the
%	     previous commit.

storage_get(Request, swish, Options) :-
	swish_reply_config(Request, Options), !.
storage_get(Request, Format, _) :-
	setting(directory, Dir),
	request_file_or_hash(Request, Dir, FileOrHash, Type),
	storage_get(Format, Dir, Type, FileOrHash, Request).

storage_get(swish, Dir, _, FileOrHash, Request) :-
	gitty_data(Dir, FileOrHash, Code, Meta),
	swish_reply([code(Code),file(FileOrHash),st_type(gitty),meta(Meta)],
		    Request).
storage_get(raw, Dir, _, FileOrHash, _Request) :-
	gitty_data(Dir, FileOrHash, Code, Meta),
	file_mime_type(Meta.name, MIME),
	format('Content-type: ~w~n~n', [MIME]),
	format('~s', [Code]).
storage_get(json, Dir, _, FileOrHash, _Request) :-
	gitty_data(Dir, FileOrHash, Code, Meta),
	reply_json_dict(json{data:Code, meta:Meta}).
storage_get(history(Depth, Includes), Dir, _, File, _Request) :-
	gitty_history(Dir, File, History, [depth(Depth),includes(Includes)]),
	reply_json_dict(History).
storage_get(history(Depth), Dir, _, File, _Request) :-
	gitty_history(Dir, File, History, [depth(Depth)]),
	reply_json_dict(History).
storage_get(diff(RelTo), Dir, _, File, _Request) :-
	gitty_diff(Dir, RelTo, File, Diff),
	reply_json_dict(Diff).

request_file_or_hash(Request, Dir, FileOrHash, Type) :-
	option(path_info(FileOrHash), Request),
	(   gitty_file(Dir, FileOrHash, _Hash)
	->  Type = file
	;   is_sha1(FileOrHash)
	->  Type = hash
	;   http_404([], Request)
	).

is_sha1(SHA1) :-
	atom_length(SHA1, 40),
	atom_codes(SHA1, Codes),
	maplist(hex_digit, Codes).

hex_digit(C) :- between(0'0, 0'9, C), !.
hex_digit(C) :- between(0'a, 0'f, C).

%%	authentity(+Request, -Authentity:dict) is det.
%
%	Provide authentication meta-information.  Currently user by
%	exploiting the pengine authentication hook and peer.

authentity(Request, Authentity) :-
	phrase(authentity(Request), Pairs),
	dict_pairs(Authentity, _, Pairs).

authentity(Request) -->
	(user(Request)->[];[]),
	(peer(Request)->[];[]).

:- multifile
	pengines:authentication_hook/3.

user(Request) -->
	{ pengines:authentication_hook(Request, swish, User),
	  ground(User)
	},
	[ user-User ].
peer(Request) -->
	{ http_peer(Request, Peer) },
	[ peer-Peer ].

%%	random_filename(-Name) is det.
%
%	Return a random file name from plain nice ASCII characters.

random_filename(Name) :-
	length(Chars, 8),
	maplist(random_char, Chars),
	atom_chars(Name, Chars).

from('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ').

random_char(Char) :-
	from(From),
	atom_length(From, Len),
	Max is Len - 1,
	random_between(0, Max, I),
	sub_atom(From, I, 1, _, Char).


		 /*******************************
		 *	    INTERFACE		*
		 *******************************/

%%	storage_file(?File) is semidet.
%%	storage_file(?File, -Data, -Meta) is semidet.
%
%	True if File is known in the store.

storage_file(File) :-
	setting(directory, Dir),
	gitty_file(Dir, File, _Head).

storage_file(File, Data, Meta) :-
	setting(directory, Dir),
	gitty_data(Dir, File, Data, Meta).


		 /*******************************
		 *	 SEARCH SUPPORT		*
		 *******************************/

:- multifile
	swish_search:typeahead/4.	% +Set, +Query, -Match, +Options

%%	swish_search:typeahead(+Set, +Query, -Match) is nondet.
%
%	Find files using typeahead  from  the   SWISH  search  box. This
%	version defines the following sets:
%
%	  - file: Search the store for matching file names, matching tag
%	    or title.
%	  - store_content: Search the content of the store for matching
%	    lines.
%
%	@tbd caching?
%	@tbd We should only demand public on public servers.

swish_search:typeahead(file, Query, FileInfo, _Options) :-
	setting(directory, Dir),
	gitty_file(Dir, File, Head),
	gitty_commit(Dir, Head, Meta),
	Meta.get(public) == true,
	(   sub_atom(File, 0, _, _, Query) % find only public
	->  true
	;   meta_match_query(Query, Meta)
	->  true
	),
	FileInfo = Meta.put(_{type:"store", file:File}).

meta_match_query(Query, Meta) :-
	member(Tag, Meta.get(tags)),
	sub_atom(Tag, 0, _, _, Query).
meta_match_query(Query, Meta) :-
	sub_atom(Meta.get(author), 0, _, _, Query).
meta_match_query(Query, Meta) :-
	Title = Meta.get(title),
	sub_atom_icasechk(Title, Start, Query),
	(   Start =:= 0
	->  true
	;   Before is Start-1,
	    sub_atom(Title, Before, 1, _, C),
	    \+ char_type(C, csym)
	).

swish_search:typeahead(store_content, Query, FileInfo, Options) :-
	limit(25, search_store_content(Query, FileInfo, Options)).

search_store_content(Query, FileInfo, Options) :-
	setting(directory, Dir),
	gitty_file(Dir, File, Head),
	gitty_data(Dir, Head, Data, Meta),
	Meta.get(public) == true,
	limit(5, search_file(File, Meta, Data, Query, FileInfo, Options)).

search_file(File, Meta, Data, Query, FileInfo, Options) :-
	split_string(Data, "\n", "\r", Lines),
	nth1(LineNo, Lines, Line),
	match(Line, Query, Options),
	FileInfo = Meta.put(_{type:"store", file:File,
			      line:LineNo, text:Line, query:Query
			     }).
