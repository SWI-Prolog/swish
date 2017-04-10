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
:- use_module(library(occurs)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).

/** <module> Bootstrap form generator

This library provides HTML rules for constructing Bootstrap forms.
*/

%!  bt_form(+Contents, +Options)//
%
%   Emit a Bootstrap form from Contents.  Each element of Contents is
%   one of the following terms:
%
%     - input(Name, Type, InputOptions)
%     - select(Name, Values, SelectOptions)
%     - checkboxes(Name, Values, BoxOptions)
%     - button(Name, Type, ButtonOptions)
%     - button_group(Buttons, GroupOptions)
%     - hidden(Name, Value)
%
%   Options processed:
%
%     - class(+Class)
%     One of 'form-inline' or 'form-horizontal'.  Default is a vertical
%     form.
%     - label_columns(+SizeCount)
%     Number of columns of the grid to use for the label.  In use for
%     'form-horizontal'.  Default is sm-2 (a pair)

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


%!  form_style(+Style, +Options)

form_style(horizontal, Options) :-
    option(class(Class), Options),
    sub_term('form-horizontal', Class).
form_style(inline, Options) :-
    option(class(Class), Options),
    sub_term('form-inline', Class).
form_style(vertical, Options) :-
    (   option(class(Class), Options)
    ->  \+ sub_term('form-inline', Class),
        \+ sub_term('form-horizontal', Class)
    ;   true
    ).


%!  bt_form_element(+Term, +Options)//
%
%   Add a single element to the form.

bt_form_element(input(Name, Type, IOptions), Options) -->
    html(div(class('form-group'),
             [ \bt_label(Name, IOptions, Options),
               \bt_input(Name, Type, IOptions, Options)
             ])).
bt_form_element(select(Name, Values, IOptions), Options) -->
    html(div(class('form-group'),
             [ \bt_label(Name, IOptions, Options),
               \bt_select(Name, Values, IOptions, Options)
             ])).
bt_form_element(checkboxes(Name, Values, IOptions), Options) -->
    html(div(class('form-group'),
             [ \bt_label(Name, IOptions, Options),
               \bt_checkboxes(Name, Values, IOptions, Options)
             ])).
bt_form_element(textarea(Name, IOptions), Options) -->
    html(div(class('form-group'),
             [ \bt_label(Name, IOptions, Options),
               \bt_textarea(Name, IOptions, Options)
             ])).
bt_form_element(button(Name, Type, IOptions), Options) -->
    bt_button(Name, Type, IOptions, Options).
bt_form_element(button_group(Buttons, IOptions), Options) -->
    bt_button_group(Buttons, IOptions, Options).
bt_form_element(hidden(Name, Value), _Options) -->
    html(input([type(hidden),name(Name),value(Value)])).

%!  bt_label(+Name, +ElementOptions, +FormsOptions)//
%
%   Emit a label.

bt_label(Name, IOptions, Options) -->
    { phrase(label_attr(Options), Attrs) },
    html(label([for(Name)|Attrs], \label(Name, IOptions))).

label_attr(Options) -->
    { form_style(horizontal, Options),
      option(label_columns(Size-Count), Options, sm-2),
      atomic_list_concat([col,Size,Count], -, Class)
    },
    [ class(['control-label', Class]) ].
label_attr(_) --> [].



%!  bt_input(+Name, +Type, +InputOptions, +FormOptions)//
%
%   Emit an input element.  InputOptions are:
%
%     - value(+Value)
%     Initial value of the input
%     - disabled(+Boolean)
%     If `true`, the input is inactive.
%     - readonly(+Boolean)
%     If `true`, the input cannot be edited.

:- html_meta(horizontal_input(html, +, +, +, ?, ?)).

bt_input(Name, Type, InputOptions, FormOptions) -->
    horizontal_input(\bt_input_elem(Name, Type, InputOptions, FormOptions),
                     [], [], FormOptions),
    !.
bt_input(Name, Type, InputOptions, FormOptions) -->
    bt_input_elem(Name, Type, InputOptions, FormOptions).

horizontal_input(HTML, Classes, Attrs, FormOptions) -->
    { form_style(horizontal, FormOptions),
      option(label_columns(Size-Count), FormOptions, sm-2),
      FieldCols is 12-Count,
      atomic_list_concat([col,Size,FieldCols], -, Class)
    },
    html(div([class([Class|Classes])|Attrs], HTML)).

bt_input_elem(Name, checkbox, InputOptions, _FormOptions) -->
    !,
    { phrase(checkbox_attr(InputOptions), Attrs) },
    html(input([type(checkbox), name(Name)|Attrs])).
bt_input_elem(Name, Type, InputOptions, _FormOptions) -->
    { phrase(input_attr(InputOptions), Attrs),
      phrase(classes(InputOptions), Classes),
      list_to_set(['form-control'|Classes], InputClasses)
    },
    html(input([type(Type), class(InputClasses), name(Name)|Attrs])).

checkbox_attr(Options) -->
    ( checkbox_value(Options) -> [] ; [] ),
    ( input_disabled(Options) -> [] ; [] ),
    ( input_readonly(Options) -> [] ; [] ),
    data(Options).

checkbox_value(Options) -->
    { option(value(true), Options) },
    [ checked(checked) ].

input_attr(Options) -->
    ( input_value(Options) -> [] ; [] ),
    ( input_disabled(Options) -> [] ; [] ),
    data(Options).

input_value(Options) -->
    { option(value(Value), Options) },
    [ value(Value) ].
input_disabled(Options) -->
    { option(disabled(true), Options) },
    [ disabled(disabled) ].
input_readonly(Options) -->
    { option(readonly(true), Options) },
    [ readonly(readonly) ].

%!  bt_select(+Name, +Values, +SelectOptions, +FormOptions)//
%
%   Emit a <select> element.  SelectOptions:
%
%     - value(Value)
%       If present, provides the preselected value
%     - size(Size)
%       Provides the number of visible options.

bt_select(Name, Values, SelectOptions, FormOptions) -->
    horizontal_input(\bt_select_elem(Name, Values, SelectOptions, FormOptions),
                     [], [], FormOptions).

bt_select_elem(Name, Values, SelectOptions, _FormOptions) -->
    { option(value(Value), SelectOptions, _),
      phrase(( (select_size(SelectOptions)     -> [] ; []),
               (select_multiple(SelectOptions) -> [] ; [])
             ), Opts)
    },
    html(select([name(Name),class('form-control')|Opts],
                \select_options(Values, Value, SelectOptions))).

select_size(Options) -->
    { option(size(Size), Options) },
    [ size(Size) ].
select_multiple(Options) -->
    { option(multiple(true), Options) },
    [ multiple(multiple) ].

select_options([], _, _) -->
    [].
select_options([H|T], Value, Options) -->
    select_option_1(H, Value, Options),
    select_options(T, Value, Options).

select_option_1(Value, Selected, _Options) -->
    { (atom(Value) ; string(Value)),
      !,
      name_label(Value, Label),
      (   Value == Selected
      ->  Opts = [selected(selected)]
      ;   Opts = []
      )
    },
    html(option([value(Value)|Opts], Label)).
select_option_1(Value, _, _) -->
    { domain_error(bt_select_option, Value) }.

%!  bt_checkboxes(+Name, +Values, +SelectOptions, +FormOptions)//
%
%   Set of checkboxes reported as an array that is a subset of Values.

bt_checkboxes(Name, Values, CBOptions, FormOptions) -->
    horizontal_input(\checkboxes(Values, CBOptions),
                     [checkboxes, array], name(Name), FormOptions).

checkboxes([], _) -->
    [].
checkboxes([H|T], Options) -->
    checkbox(H, Options),
    checkboxes(T, Options).

checkbox(Value, Options) -->
    { name_label(Value, Label),
      (   option(value(Selected), Options),
          memberchk(Value, Selected)
      ->  Opts = [checked(checked)]
      ;   Opts = []
      )
    },
    html(label(class('checkbox-inline'),
               [ input([ type(checkbox), name(Value), autocomplete(false)
                       | Opts
                       ]),
                 Label
               ])).

%!  bt_textarea(+Name, +TextAreaOptions, +FormOptions)//

bt_textarea(Name, TextAreaOptions, FormOptions) -->
    horizontal_input(\bt_textarea_elem(Name, TextAreaOptions, FormOptions),
                     [], [], FormOptions),
    !.
bt_textarea(Name, TextAreaOptions, FormOptions) -->
    bt_textarea_elem(Name, TextAreaOptions, FormOptions).


bt_textarea_elem(Name, TextAreaOptions, _FormOptions) -->
    { option(rows(Rows), TextAreaOptions, 4)
    },
    html(textarea([ class('form-control)'),
                    rows(Rows),
                    name(Name),
                    style('width:100%')
                  ], [])).


%!  bt_button_group(+Buttons, +ButtonOptions, +FormOptions)//
%
%   Emit a div for a group of buttons.

bt_button_group(Buttons, _ButtonOptions, FormOptions) -->
    { form_style(horizontal, FormOptions),
      !,
      option(label_columns(_-Count), FormOptions, sm-2),
      Count12 is 12-Count,
      atomic_list_concat([col,xs,offset,Count], -, Offset),
      atomic_list_concat([col,xs,Count12], -, Width)
    },
    html(div(class('form-group'),
             div(class([Offset,Width]),
                 \bt_form_content(Buttons, FormOptions)))).
bt_button_group(Buttons, _ButtonOptions, FormOptions) -->
    html(div(class(['col-xs-12', 'text-center']),
             \bt_form_content(Buttons, FormOptions))).


%!  bt_button(+Name, +Type, +ButtonOptions, +FormOptions)//

bt_button(Name, Type, IOptions, Options) -->
    { phrase(button_classes(IOptions, Options), BtnClasses),
      phrase(data(IOptions), DataAttrs)
    },
    html(button([ type(Type),
                  name(Name),
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

data_values([]) --> !.
data_values([H|T]) --> !, data_values(H), data_values(T).
data_values(Name-Value) -->
    !,
    data_value(Name, Value).
data_values(Name=Value) -->
    !,
    data_value(Name, Value).
data_values(NameValue) -->
    { NameValue =.. [Name,Value] },
    data_value(Name, Value).

data_value(Name, Value) -->
    { atom_concat('data-', Name, AttrName),
      Attr =.. [AttrName,Value]
    },
    [Attr].

%!  classes(+Options)//
%
%   Collect defined classes

classes([]) --> !.
classes([class(Classes)|T]) --> !, class(Classes), classes(T).
classes([_|T]) --> classes(T).

class([]) --> !.
class([H|T]) --> !, class(H), class(T).
class(Class) --> [Class].


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
