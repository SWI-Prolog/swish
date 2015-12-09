/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

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

:- module(swish_jquery,
	  [ jquery/3			% +Selector, +Request, -Reply
	  ]).
:- use_module(library(error)).
:- use_module(library(pengines)).

/** <module> Call jQuery on the SWISH interface

Select objects in the SWISH  interface   using  jQuery, run an arbitrary
JavaScript function on them and return   the  result. This predicate was
introduced while adding functionality to request the contents of tabs in
the SWISH interface.
*/

%%	jquery(+Selector, +Function, -Reply) is det.
%
%	Run a jQuery query in the  SWISH interface. Selector defines the
%	receiver  of  the  jQuery  method,  Function  is  the  JavaScript
%	function to run  on  the  receiver   and  Reply  is  the  Prolog
%	representation of the result.
%
%	@arg Selector selects the jQuery receiver. It takes three forms:
%
%	  - If the selector is a string, it is simply interpreted as
%	    =|$(Selector)|=.
%	  - If the selector is this(SubSelector), it perform a jQuery
%	    `find` using `SubSelector` on the Prolog runner.  Using
%	  - If the selector is swish(SubSelector), as above, but
%	    starting at the SWISH plugin instance
%
%	@arg Function is a compound term  representing a JavaScript call.
%	The functor name is used as   method and the remaining arguments
%	are converted by json_write_dict/2.
%
%	@arg Reply is the JavaScript reply, converted to Prolog by
%	the Pengine.stringify() method.

jquery(Selector, Function, Reply) :-
	map_selector(Selector, Selector1),
	compound_name_arguments(Function, Method, Args),
	pengine_input(_{ type: "jQuery",
			 selector: Selector1,
			 method: Method,
			 arguments: Args
		       },
		      Reply).

map_selector(Selector, Selector) :-
	string(Selector), !.
map_selector(Selector, Selector) :-
	atom(Selector), !.
map_selector(Selector, json{root:Name, sub:SubSelector}) :-
	compound_name_arguments(Selector, Name, Args),
	root_selector(Name),
	(   Args == []
	->  SubSelector = ""
	;   Args = [SubSelector]
	->  must_be(string, SubSelector)
	;   domain_error(arity_one, Selector)
	).

root_selector(this) :- !.
root_selector(swish) :- !.
root_selector(Selector) :-
	domain_error(root_selector, Selector).
