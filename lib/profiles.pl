/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, VU University Amsterdam
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

:- module(swish_profiles, []).
:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(filesex)).
:- use_module(library(dcg/basics)).

:- multifile
	user:file_search_path/2,
	swish_config:config/2,
	swish_config:source_alias/2.

% make profile(File) find the example data
user:file_search_path(profile, swish(profiles)).
% make SWISH serve /profile/File as profile(File).
swish_config:source_alias(profile, [access(read), search('*.{pl,swinb}')]).

		 /*******************************
		 *	    SWISH CONFIG	*
		 *******************************/

%%	swish_config:config(-Name, -Profiles) is det.
%
%	Provides the object `config.swish.profiles`, a  JSON object that
%	provides the available profiles.

swish_config:config(profiles, Profiles) :-
	findall(Profile, swish_profile(Profile), Profiles0),
	sort(value, =<, Profiles0, Profiles1),
	join_profiles(Profiles1, Profiles).

join_profiles([], []).
join_profiles([P1,P2|T0], [P|T]) :-
	join_profiles(P1, P2, P), !,
	join_profiles(T0, T).
join_profiles([P|T0], [P1|T]) :-
	P1 = P.put(type, [P.type]),
	join_profiles(T0, T).

join_profiles(P1, P2, profile{value:Name, type:[Ext1,Ext2],
			      label:Label, title:Title}) :-
	P1 >:< _{value:Name, type:Ext1, label:Label1, title:Title1},
	P2 >:< _{value:Name, type:Ext2, label:Label2, title:Title2},
	join_value(Label1, Label2, Label),
	join_value(Title1, Title2, Title).

join_value(V, V, V) :- !.
join_value(V, "No title", V) :- !.
join_value("No title", V, V) :- !.
join_value(V, _, V).

swish_profile(profile{value:Name, type:Ext, label:Label, title:Title}) :-
	absolute_file_name(profile(.), Dir,
			   [ file_type(directory),
			     access(read),
			     solutions(all)
			   ]),
	directory_file_path(Dir, '*.{pl,swinb}', Pattern),
	expand_file_name(Pattern, Files),
	member(FilePath, Files),
	file_base_name(FilePath, File),
	file_name_extension(Name, Ext, File),
	value_label(Name, Label),
	title(FilePath, Title).

value_label(Value, Label) :-
	atom_codes(Value, Codes),
	phrase(label(Label), Codes).

label(Label) -->
	string(_), "-", !, rest(Codes),
	{ string_codes(Label, Codes) }.
label(Label) -->
	rest(Codes),
	{ string_codes(Label, Codes) }.

title(FilePath, Title) :-
	first_line(FilePath, FirstLine),
	(   FirstLine == end_of_file
	->  Title = "Empty"
	;   phrase(title(Title), FirstLine)
	).

first_line(File, Line) :-
	setup_call_cleanup(
	    open(File, read, In),
	    read_line_to_codes(In, Line),
	    close(In)).

title(Title) -->
	"%", whites, !, rest(Codes),
	{ string_codes(Title, Codes) }.
title("No title") --> rest(_).

rest(List, List, []).
