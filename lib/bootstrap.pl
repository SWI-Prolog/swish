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

:- module(swish_bootstrap,
          [ bt_form//2,                 % +Fields, +Options
            bt_button//4,               % +Name, +Type, +IOptions, +Options

            name_label/2                % +Name, -Label
          ]).
:- use_module(library(option)).
:- use_module(library(http/html_write)).

/** <module> Bootstrap form generator

This library provides HTML rules for constructing Bootstrap forms.
*/

%!  bt_form(+Fields, +Options)//
%
%   Emit a Bootstrap form from Fields.  Each field is a term
%
%     input(Name, InputOptions)
%
%   Options processed:
%
%     - class(+Class)
%     One of 'form-inline' or 'form-horizontal'.  Default is a vertical
%     form.

bt_form(Fields, Options) -->
    { form_attributes(Atts, Options) },
    html(form(Atts, \bt_form_content(Fields, Options))).

form_attributes([class(Class)], Options) :-
    option(class(Class), Options).
form_attributes([], _).

bt_form_content([], _) --> [].
bt_form_content([H|T], Options) -->
    bt_form_element(H, Options),
    bt_form_content(T, Options).


%!  bt_form_element(+Term, +Options)//
%
%   Add a single element to the form.

bt_form_element(input(Name, Type, IOptions), Options) -->
    html(div(class('form-group'),
             [ \bt_label(Name, IOptions, Options),
               \bt_input(Name, Type, IOptions, Options)
             ])).
bt_form_element(button(Name, Type, IOptions), Options) -->
    bt_button(Name, Type, IOptions, Options).

%!  bt_label(+Name, +ElementOptions, +FormsOptions)//
%
%   Emit a label.

bt_label(Name, IOptions, _Options) -->
    html(label([for(Name)], \label(Name, IOptions))).

%!  bt_input(+Name, +Type, +InputOptions, +FormOptions)//
%
%   Emit an input element

bt_input(Name, Type, _InputOptions, _FormOptions) -->
    html(input([type(Type), class('form-control'), name(Name)])).

%!  bt_button(+Name, +Type, +ButtonOptions, +FormOptions)//

bt_button(Name, Type, IOptions, Options) -->
    { phrase(button_classes(IOptions, Options), BtnClasses),
      phrase(data(IOptions), DataAttrs)
    },
    html(button([ type(Type),
                  class([btn|BtnClasses])
                | DataAttrs
                ],
                \label(Name, IOptions))).

%!  button_classes(+ElemOptions, +FormOptions)//
%
%   Collect the classes from element and form options

button_classes(IOptions, Options) -->
    button_type_class(IOptions),
    button_size_class(IOptions, Options).

button_type_class(IOptions) -->
    { option(type(Type), IOptions),
      !,
      atom_concat('btn-', Type, Class)
    },
    [Class].
button_type_class(_IOptions) -->
    ['btn-default'].

button_size_class(IOptions, Options) -->
    { (   option(button_size(Size), IOptions)
      ;   option(button_size(Size), Options)
      ),
      !,
      atom_concat('btn-', Size, Class)
    },
    [Class].
button_size_class(_IOptions, _Options) -->
    [].

%!  data(+Options)//
%
%   Collect data options.

data(Options) -->
    { option(data(Data), Options, []) },
    !,
    data_values(Data).
data(_) --> [].

data_values([]) --> [].
data_values([H|T]) --> data_value(H), data_values(T).

data_value(Name-Value) -->
    !,
    data_value(Name, Value).
data_value(Name=Value) -->
    !,
    data_value(Name, Value).
data_value(NameValue) -->
    { NameValue =.. [Name,Value] },
    data_value(Name, Value).

data_value(Name, Value) -->
    { atom_concat('data-', Name, AttrName),
      Attr =.. [AttrName,Value]
    },
    [Attr].


		 /*******************************
		 *            UTIL		*
		 *******************************/

label(Name, Options) -->
    { (   option(label(Label), Options)
      ->  true
      ;   name_label(Name, Label)
      )
    },
    html(Label).

%!  name_label(+Name, -Label) is det.
%
%   Determine a label from a name by   upcasing  the first character and
%   replacing all underscores by spaces.

name_label(Name, Label) :-
    atom_codes(Name, Codes),
    phrase(name_label(up, LCodes), Codes),
    atom_codes(Label, LCodes).

name_label(up,   [H|T])    --> [H0], !, {code_type(H, to_upper(H0))}, name_label(keep, T).
name_label(keep, [0'\s|T]) --> "_",  !, name_label(keep, T).
name_label(keep, [H|T])    --> [H],  !, name_label(keep, T).
name_label(_,    [])       --> [].
