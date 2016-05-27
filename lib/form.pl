/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

:- module(swish_form,
	  [ validate_form/2,		% +Dict, +Fields
	    validate_field/4,		% +Dict, +Field, -Value, +Options
	    input_error/2		% +Field, +Error
	  ]).

/** <module> Form handling utilities

This module simplifies handling input from forms in the SWISH interface.
The  values  from  a  form  can   be    turned   into  an  object  using
`notebook.formData(form)`.  The  returned  dict  may   be  passed  to  a
predicate inside SWISH.
*/


%%	validate_form(+Dict, +Fields) is det.
%
%	Run validate_field/4 on all  specified   fields,  combining  the
%	error message.  Fields is a list of terms of the form
%
%	    field(Name, Value, Options)

validate_form(Dict, Fields) :-
	validate_form(Fields, Dict, Errors),
	(   Errors == []
	->  true
	;   throw(error(form_error(Errors), _))
	).

validate_form([], _, []).
validate_form([field(Field, Value, Options)|T], Dict, Errors) :-
	catch(validate_field(Dict, Field, Value, Options),
	      error(input_error(Field, Error),_),
	      true),
	(   var(Error)
	->  Errors = Errors1
	;   Errors = [input_error(Field, Error)|Errors1]
	),
	validate_form(T, Dict, Errors1).


%%	validate_field(+Dict, +Field, -Value, +Options) is det.
%
%	True when Value is a valid value   for Field from Dict according
%	to Options. Options is  a  list   of  processing  steps taken to
%	convert and validate the value.  Defined steps are:
%
%	  - alnum
%	  All characters must be alphanumeric
%	  - alnum_and_spaces
%	  All characters must be alphanumeric or spaces
%	  - atom
%	  Convert input to an atom
%	  - default(Term)
%	  Use Term as value if no value appears in the input.
%	  - downcase
%	  Convert input to lower case
%	  - email
%	  Input must be a value E-mail address.
%	  - float
%	  Value is converted to a floating point number.
%	  - integer
%	  Value is converted to an integer.
%	  - length > N
%	  The value must have at more than N characters
%	  - length >= N
%	  The value must have at least N characters
%	  - length =< N
%	  The value must have at most N characters
%	  - length < N
%	  The value must have at less than N characters
%	  - number
%	  Value is converted to a number (integer or float)
%	  - oneof(List)
%	  Input must be a member of List.
%	  - password
%	  Input must be a reasonable password.
%	  - strip
%	  Strip leading and trailing white space and normalize
%	  internal white space to a single space.


validate_field(Dict, Field, Value, Options) :-
	(   Value0 = Dict.get(Field)
	->  validate_value(Options, Value0, Value, Field)
	;   memberchk(default(Value), Options)
	->  true
	;   input_error(Field, required)
	).

validate_value([], Value, Value, _).
validate_value([H|T], Value0, Value, Field) :-
	(   validate_step(H, Value0, Value1)
	->  true
	;   validate_failed(H, Value0, Field)
	),
	validate_value(T, Value1, Value, Field).

%%	validate_step(+Step, +Value0, -Value) is semidet.

validate_step(alnum, Value, Value) :-
	forall(sub_atom(Value, _, 1, _, Char),
	       char_type(Char, alnum)).
validate_step(length >= N, Value, Value) :-
	string_length(Value, Len),
	Len >= N.
validate_step(length > N, Value, Value) :-
	string_length(Value, Len),
	Len > N.
validate_step(length < N, Value, Value) :-
	string_length(Value, Len),
	Len < N.
validate_step(length =< N, Value, Value) :-
	string_length(Value, Len),
	Len =< N.
validate_step(strip, Value0, Value) :-
	normalize_space(string(Value), Value0).
validate_step(alnum_and_spaces, Value, Value) :-
	forall(sub_atom(Value, _, 1, _, Char),
	       alnum_or_space(Char)).
validate_step(email, Value, Value) :-
	string_codes(Value, Codes),
	phrase(email, Codes).
validate_step(downcase, Value0, Value) :-
	string_lower(Value0, Value).
validate_step(atom, Value0, Value) :-
	atom_string(Value, Value0).
validate_step(number, Value0, Value) :-
	number_string(Value, Value0).
validate_step(integer, Value0, Value) :-
	number_string(Value, Value0),
	integer(Value).
validate_step(float, Value0, Value) :-
	number_string(Value1, Value0),
	Value is float(Value1).
validate_step(oneof(List), Value, Value) :-
	memberchk(Value, List).
validate_step(password, Value, Value) :-
	string_length(Value, Len),
	Len >= 6.
validate_step(default(_), Value, Value).

alnum_or_space(' ') :- !.
alnum_or_space(Char) :-
	char_type(Char, alnum).

email --> user_name, "@", domain_name.
user_name --> user_name_char, user_name_chars.
domain_name --> domain_name_segment, ".", domain_name_segments.

user_name_chars --> user_name_char, !, user_name_chars.
user_name_chars --> "".

user_name_char -->
	[C],
	{ between(1, 127, C),
	  (   code_type(C, alnum)
	  ->  true
	  ;   name_special(C)
	  )
	}.

name_special(0'.).
name_special(0'-).

domain_name_segment --> domain_name_char, domain_name_chars.
domain_name_segments -->
	domain_name_segment,
	(   "."
	->  domain_name_segments
	;   ""
	).

domain_name_chars --> domain_name_char, !, domain_name_chars.
domain_name_chars --> "".

domain_name_char -->
	[C],
	{ between(1, 127, C),
	  (   code_type(C, alnum)
	  ->  true
	  ;   domain_special(C)
	  )
	}.

domain_special(0'-).
domain_special(0'_).

validate_failed(H, _Value0, Field) :-
	input_error(Field, H).

input_error(Field, Error) :-
	throw(error(input_error(Field, Error), _)).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(input_error(Field, Expected)) -->
	[ '~w: '-[Field] ],
	expected(Expected).
prolog:error_message(form_error(Errors)) -->
	field_errors(Errors).

field_errors([]) --> [].
field_errors([H|T]) -->
	prolog:error_message(H),
	(   {T==[]}
	->  []
	;   [nl],
	    field_errors(T)
	).


expected(oneof(List)) --> !,
	[ 'One of '-[] ],
	oneof(List).
expected(required) --> !,
	[ 'This field is required'-[] ].
expected(length > N) --> !,
	[ 'Needs at more than ~d characters'-[N] ].
expected(length >= N) --> !,
	[ 'Needs at least ~d characters'-[N] ].
expected(length =< N) --> !,
	[ 'Needs at most ~d characters'-[N] ].
expected(length < N) --> !,
	[ 'Needs less than ~d characters'-[N] ].
expected(matching_password) -->
	[ 'The password does not match'-[] ].
expected(new_user) -->
	[ 'A user with this name already exists'-[] ].
expected(Expected) --> !,
	[ 'This field must hold a valid ~w'-[Expected] ].

oneof([One]) --> !,
	[ '~w'-[One] ].
oneof([One, Two]) --> !,
	[ '~w or ~w'-[One, Two] ].
oneof([H|T]) -->
	[ '~w, '-[H] ],
	oneof(T).
