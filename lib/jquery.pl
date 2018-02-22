/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(swish_jquery:jquery(_,_,_)).
