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

:- module(swish_page,
	  [ swish_reply/2,			% +Options, +Request
	    swish_page//1,			% +Options

	    swish_navbar//1,			% +Options
	    swish_content//1,			% +Options

	    swish_resources//0,
	    swish_js//0,
	    swish_css//0
	  ]).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_header)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- if(exists_source(library(http/http_ssl_plugin))).
:- use_module(library(http/http_ssl_plugin)).
:- endif.
:- use_module(library(debug)).
:- use_module(library(time)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(library(error)).
:- use_module(library(http/http_client)).

:- use_module(config).
:- use_module(help).
:- use_module(form).
:- use_module(search).

/** <module> Provide the SWISH application as Prolog HTML component

This library provides the SWISH page  and   its  elements as Prolog HTML
grammer rules. This allows for server-side   generated  pages to include
swish or parts of swish easily into a page.
*/

http:location(pldoc, swish(pldoc), [priority(100)]).

:- http_handler(swish(.), swish_reply([]), [id(swish), prefix]).

:- multifile
	swish_config:source_alias/2,
	swish_config:reply_page/1,
	swish_config:verify_write_access/3, % +Request, +File, +Options
	swish_config:authenticate/2.	    % +Request, -User

%%	swish_reply(+Options, +Request)
%
%	HTTP handler to reply the  default   SWISH  page.  Processes the
%	following parameters:
%
%	  - code(Code)
%	  Use Code as initial code. Code is either an HTTP url or
%	  concrete source code.
%	  - background(Code)
%	  Similar to Code, but not displayed in the editor.
%	  - examples(Code)
%	  Provide examples. Each example starts with ?- at the beginning
%	  of a line.
%	  - q(Query)
%	  Use Query as the initial query.

swish_reply(_Options, Request) :-
	swish_config:authenticate(Request, _User), % must throw to deny access
	fail.
swish_reply(Options, Request) :-
	option(method(Method), Request),
	Method \== get, !,
	swish_rest_reply(Method, Request, Options).
swish_reply(_, Request) :-
	serve_resource(Request), !.
swish_reply(_, Request) :-
	swish_reply_config(Request), !.
swish_reply(SwishOptions, Request) :-
	Params = [ code(_,	 [optional(true)]),
		   background(_, [optional(true)]),
		   examples(_,   [optional(true)]),
		   q(_,          [optional(true)]),
		   format(_,     [oneof([swish,raw]), default(swish)])
		 ],
	http_parameters(Request, Params),
	params_options(Params, Options0),
	merge_options(Options0, SwishOptions, Options1),
	source_option(Request, Options1, Options2),
	swish_reply1(Options2).

swish_reply1(Options) :-
	option(code(Code), Options),
	option(format(raw), Options), !,
	format('Content-type: text/x-prolog~n~n'),
	format('~s', [Code]).
swish_reply1(Options) :-
	swish_config:reply_page(Options), !.
swish_reply1(Options) :-
	reply_html_page(
	    swish(main),
	    [ title('SWISH -- SWI-Prolog for SHaring'),
	      link([ rel('shortcut icon'),
		     href('/icons/favicon.ico')
		   ]),
	      link([ rel('apple-touch-icon'),
		     href('/icons/swish-touch-icon.png')
		   ])
	    ],
	    \swish_page(Options)).

params_options([], []).
params_options([H0|T0], [H|T]) :-
	arg(1, H0, Value), nonvar(Value), !,
	functor(H0, Name, _),
	H =.. [Name,Value],
	params_options(T0, T).
params_options([_|T0], T) :-
	params_options(T0, T).


%%	source_option(+Request, +Options0, -Options)
%
%	If the data was requested  as   '/Alias/File',  reply using file
%	Alias(File).

source_option(_Request, Options0, Options) :-
	option(code(Code), Options0),
	option(format(swish), Options0), !,
	(   uri_is_global(Code)
	->  Options = [url(Code)|Options0]
	;   Options = Options0
	).
source_option(Request, Options0, Options) :-
	source_file(Request, File, Options0), !,
	option(path(Path), Request),
	(   source_data(File, String, Options1)
	->  append([ [code(String), url(Path)],
		     Options1,
		     Options0
		   ], Options)
	;   http_404([], Request)
	).
source_option(_, Options, Options).

%%	source_file(+Request, -File, +Options) is semidet.
%
%	File is the file associated with a SWISH request.  A file is
%	associated if _path_info_ is provided.  If the file does not
%	exist, an HTTP 404 exception is returned.  Options:
%
%	  - alias(-Alias)
%	    Get the swish_config:source_alias/2 Alias name that
%	    was used to find File.

source_file(Request, File, Options) :-
	option(path_info(PathInfo), Request), !,
	(   path_info_file(PathInfo, File, Options)
	->  true
	;   http_404([], Request)
	).

path_info_file(PathInfo, Path, Options) :-
	sub_atom(PathInfo, B, _, A, /),
	sub_atom(PathInfo, 0, B, _, Alias),
	sub_atom(PathInfo, _, A, 0, File),
	catch(swish_config:source_alias(Alias, AliasOptions), E,
	      (print_message(warning, E), fail)),
	Spec =.. [Alias,File],
	http_safe_file(Spec, []),
	absolute_file_name(Spec, Path,
			   [ access(read),
			     file_errors(fail)
			   ]),
	confirm_access(Path, AliasOptions), !,
	option(alias(Alias), Options, _).

source_data(Path, Code, [title(Title), type(Ext)]) :-
	setup_call_cleanup(
	    open(Path, read, In, [encoding(utf8)]),
	    read_string(In, _, Code),
	    close(In)),
	file_base_name(Path, File),
	file_name_extension(Title, Ext, File).

confirm_access(Path, Options) :-
	option(if(Condition), Options), !,
	must_be(oneof([loaded]), Condition),
	eval_condition(Condition, Path).
confirm_access(_, _).

eval_condition(loaded, Path) :-
	source_file(Path).

%%	serve_resource(+Request) is semidet.
%
%	Serve /swish/Resource files.

serve_resource(Request) :-
	option(path_info(Info), Request),
	resource_prefix(Prefix),
	sub_atom(Info, 0, _, _, Prefix), !,
	http_reply_file(swish_web(Info), [], Request).

resource_prefix('css/').
resource_prefix('help/').
resource_prefix('form/').
resource_prefix('icons/').
resource_prefix('js/').
resource_prefix('bower_components/').

%%	swish_page(+Options)//
%
%	Generate the entire SWISH default page.

swish_page(Options) -->
	swish_navbar(Options),
	swish_content(Options).

%%	swish_navbar(+Options)//
%
%	Generate the swish navigation bar.

swish_navbar(Options) -->
	swish_resources,
	html(nav([ class([navbar, 'navbar-default']),
		   role(navigation)
		 ],
		 [ div(class('navbar-header'),
		       [ \collapsed_button,
			 \swish_logos(Options)
		       ]),
		   div([ class([collapse, 'navbar-collapse']),
			 id(navbar)
		       ],
		       [ ul([class([nav, 'navbar-nav'])], []),
			 \search_form(Options)
		       ])
		 ])).

collapsed_button -->
	html(button([type(button),
		     class('navbar-toggle'),
		     'data-toggle'(collapse),
		     'data-target'('#navbar')
		    ],
		    [ span(class('sr-only'), 'Toggle navigation'),
		      span(class('icon-bar'), []),
		      span(class('icon-bar'), []),
		      span(class('icon-bar'), [])
		    ])).

swish_logos(Options) -->
	pengine_logo(Options),
	swish_logo(Options).

pengine_logo(_Options) -->
	{ http_absolute_location(root(.), HREF, [])
	},
	html(a([href(HREF), class('pengine-logo')], &(nbsp))).
swish_logo(_Options) -->
	{ http_absolute_location(swish('index.html'), HREF, [])
	},
	html(a([href(HREF), class('swish-logo')], &(nbsp))).

%%	search_form(+Options)//
%
%	Add search box to the navigation bar

search_form(Options) -->
	html(div(class(['col-sm-3', 'col-md-3', 'pull-right']),
		 \search_box(Options))).


%%	swish_content(+Options)//
%
%	Generate the SWISH editor, Prolog output  area and query editor.
%	Options processed:
%
%	  - source(HREF)
%	  Load initial source from HREF

swish_content(Options) -->
	{ document_type(Type, Options)
	},
	swish_resources,
	swish_config_hash,
	html(div([id(content), class([container, swish])],
		 [ div([class([tile, horizontal]), 'data-split'('50%')],
		       [ div([ class([editors, tabbed])
			     ],
			     [ div([ class(['prolog-editor']),
				     'data-label'('Program')
				   ],
				   \source(Type, Options))
			     | \notebooks(Type, Options)
			     ]),
			 div([class([tile, vertical]), 'data-split'('70%')],
			     [ div(class('prolog-runners'), []),
			       div(class('prolog-query'), \query(Options))
			     ])
		       ]),
		   \background(Options),
		   \examples(Options)
		 ])).


%%	swish_config_hash//
%
%	Set `window.swish.config_hash` to a  hash   that  represents the
%	current configuration. This is used by   config.js  to cache the
%	configuration in the browser's local store.

swish_config_hash -->
	{ swish_config_hash(Hash) },
	js_script({|javascript(Hash)||
		   window.swish = window.swish||{};
		   window.swish.config_hash = Hash;
		   |}).


%%	source(+Type, +Options)//
%
%	Associate the source with the SWISH   page. The source itself is
%	stored  in  the  textarea  from  which  CodeMirror  is  created.
%	Options:
%
%	  - code(+String)
%	  Initial code of the source editor
%	  - file(+File)
%	  If present and code(String) is present, also associate the
%	  editor with the given file.  See storage.pl.

source(pl, Options) -->
	{ option(code(Spec), Options), !,
	  download_source(Spec, Source, Options),
	  phrase(source_data_attrs(Options), Extra)
	},
	source_meta_data(Options),
	html(textarea([ class([source,prolog]),
			style('display:none')
		      | Extra
		      ],
		      Source)).
source(_, _) --> [].

source_data_attrs(Options) -->
	(source_file_data(Options) -> [] ; []),
	(source_url_data(Options) -> [] ; []),
	(source_title_data(Options) -> [] ; []).

source_file_data(Options) -->
	{ option(file(File), Options) },
	['data-file'(File)].
source_url_data(Options) -->
	{ option(url(URL), Options) },
	['data-url'(URL)].
source_title_data(Options) -->
	{ option(title(File), Options) },
	['data-title'(File)].


%%	source_meta_data(+Options)//
%
%	Dump the meta-data of the provided file into swish.meta_data.

source_meta_data(Options) -->
	{ option(file(_), Options),
	  option(meta(Meta), Options)
	}, !,
	js_script({|javascript(Meta)||
		   window.swish = window.swish||{};
		   window.swish.meta_data = Meta;
		   |}).
source_meta_data(_) --> [].

background(Options) -->
	{ option(background(Spec), Options), !,
	  download_source(Spec, Source, Options)
	},
	html(textarea([ class([source,prolog,background]),
			style('display:none')
		      ],
		      Source)).
background(_) --> [].


examples(Options) -->
	{ option(examples(Examples), Options), !
	},
	html(textarea([ class([examples,prolog]),
			style('display:none')
		      ],
		      Examples)).
examples(_) --> [].


query(Options) -->
	{ option(q(Query), Options)
	}, !,
	html(textarea([ class([query,prolog]),
			style('display:none')
		      ],
		      Query)).
query(_) --> [].

%%	notebooks(+Type, +Options)//
%
%	We have opened a notebook. Embed the notebook data in the
%	left-pane tab area.

notebooks(swinb, Options) -->
	{ option(code(Spec), Options),
	  download_source(Spec, NoteBookText, Options),
	  phrase(source_data_attrs(Options), Extra)
	},
	html(div([ class('notebook'),
		   'data-label'('Notebook')		% Use file?
		 ],
		 [ pre([ class('notebook-data'),
			 style('display:none')
		       | Extra
		       ],
		       NoteBookText),
		   \source_meta_data(Options)
		 ])).
notebooks(_, _) --> [].

%%	download_source(+HREF, -Source, +Options) is det.
%
%	Download source from a URL.  Options processed:
%
%	  - timeout(+Seconds)
%	    Max time to wait for reading the source.  Default
%	    is 10 seconds.
%	  - max_length(+Chars)
%	    Maximum lenght of the content.  Default is 1 million.
%	  - encoding(+Encoding)
%	    Encoding used to interpret the text.  Default is UTF-8.
%
%	@bug: Should try to interpret the encoding from the HTTP
%	      header.

download_source(HREF, Source, Options) :-
	uri_is_global(HREF), !,
	option(timeout(TMO), Options, 10),
	option(max_length(MaxLen), Options, 1_000_000),
	catch(call_with_time_limit(
		  TMO,
		  setup_call_cleanup(
		      http_open(HREF, In,
				[ cert_verify_hook(cert_accept_any)
				]),
		      read_source(In, MaxLen, Source, Options),
		      close(In))),
	      E, load_error(E, Source)).
download_source(Source0, Source, Options) :-
	option(max_length(MaxLen), Options, 1_000_000),
	string_length(Source0, Len),
	(   Len =< MaxLen
	->  Source = Source0
	;   format(string(Source),
		   '% ERROR: Content too long (max ~D)~n', [MaxLen])
	).

read_source(In, MaxLen, Source, Options) :-
	option(encoding(Enc), Options, utf8),
	set_stream(In, encoding(Enc)),
	ReadMax is MaxLen + 1,
	read_string(In, ReadMax, Source0),
	string_length(Source0, Len),
	(   Len =< MaxLen
	->  Source = Source0
	;   format(string(Source),
		   ' % ERROR: Content too long (max ~D)~n', [MaxLen])
	).

load_error(E, Source) :-
	message_to_string(E, String),
	format(string(Source), '% ERROR: ~s~n', [String]).

%%	document_type(-Type, +Options) is det.
%
%	Determine the type of document.
%
%	@arg Type is one of `notebook` or `prolog`

document_type(Type, Options) :-
	(   option(type(Type0), Options)
	->  Type = Type0
	;   option(meta(Meta), Options),
	    file_name_extension(_, Type0, Meta.name),
	    Type0 \== ''
	->  Type = Type0
	;   Type = pl
	).


		 /*******************************
		 *	     RESOURCES		*
		 *******************************/

%%	swish_resources//
%
%	Include  SWISH  CSS  and   JavaScript.    This   does   not  use
%	html_require//1  because  we  need  to   include  the  JS  using
%	RequireJS, which requires a non-standard script element.

swish_resources -->
	swish_css,
	swish_js.

swish_js  --> html_post(head, \include_swish_js).
swish_css --> html_post(head, \include_swish_css).

include_swish_js -->
	{ swish_resource(js, JS),
	  swish_resource(rjs, RJS),
	  http_absolute_location(swish(js/JS), SwishJS, []),
	  http_absolute_location(swish(RJS),   SwishRJS, [])
	},
	rjs_timeout(JS),
	html(script([ src(SwishRJS),
		      'data-main'(SwishJS)
		    ], [])).

rjs_timeout('swish-min') --> !,
	js_script({|javascript||
// Override RequireJS timeout, until main file is loaded.
window.require = { waitSeconds: 0 };
		  |}).
rjs_timeout(_) --> [].


include_swish_css -->
	{ swish_resource(css, CSS),
	  http_absolute_location(swish(css/CSS), SwishCSS, [])
	},
	html(link([ rel(stylesheet),
		    href(SwishCSS)
		  ])).

swish_resource(Type, ID) :-
	alt(Type, ID, File),
	(   File == (-)
	;   absolute_file_name(File, _P, [file_errors(fail), access(read)])
	), !.

alt(js,  'swish-min',     swish_web('js/swish-min.js')) :-
	\+ debugging(nominified).
alt(js,  'swish',         swish_web('js/swish.js')).
alt(css, 'swish-min.css', swish_web('css/swish-min.css')) :-
	\+ debugging(nominified).
alt(css, 'swish.css',     swish_web('css/swish.css')).
alt(rjs, 'js/require.js', swish_web('js/require.js')) :-
	\+ debugging(nominified).
alt(rjs, 'bower_components/requirejs/require.js', -).


		 /*******************************
		 *	       REST		*
		 *******************************/

%%	swish_rest_reply(+Method, +Request, +Options) is det.
%
%	Handle non-GET requests.  Such requests may be used to modify
%	source code.

swish_rest_reply(put, Request, Options) :-
	merge_options(Options, [alias(_)], Options1),
	source_file(Request, File, Options1), !,
	option(content_type(String), Request),
	http_parse_header_value(content_type, String, Type),
	read_data(Type, Request, Data, _Meta),
	verify_write_access(Request, File, Options1),
	setup_call_cleanup(
	    open(File, write, Out),
	    format(Out, '~s', [Data]),
	    close(Out)),
	reply_json_dict(true).

read_data(media(Type,_), Request, Data, Meta) :-
	http_json:json_type(Type), !,
	http_read_json_dict(Request, Dict),
	del_dict(data, Dict, Data, Meta).
read_data(media(text/_,_), Request, Data, _{}) :-
	http_read_data(Request, Data, [to(string)]).

%%	swish_config:verify_write_access(+Request, +File, +Options) is
%%	nondet.
%
%	Hook that verifies that the HTTP Request  may write to File. The
%	hook must succeed to grant access. Failure   is  is mapped to an
%	HTTP _403 Forbidden_ reply. The  hook   may  throw  another HTTP
%	reply.  By default, the following options are passed:
%
%	  - alias(+Alias)
%	    The swish_config:source_alias/2 Alias used to find File.

verify_write_access(Request, File, Options) :-
	swish_config:verify_write_access(Request, File, Options), !.
verify_write_access(Request, _File, _Options) :-
	option(path(Path), Request),
	throw(http_reply(forbidden(Path))).
