/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
