/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2017, VU University Amsterdam
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

:- module(swish_page,
	  [ swish_reply/2,			% +Options, +Request
	    swish_reply_resource/1,		% +Request
	    swish_page//1,			% +Options

	    swish_navbar//1,			% +Options
	    swish_content//1,			% +Options

	    pengine_logo//1,			% +Options
	    swish_logo//1,			% +Options

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
:- use_module(library(http/json)).
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
:- use_module(search).
:- use_module(chat).
:- use_module(authenticate).
:- use_module(pep).

/** <module> Provide the SWISH application as Prolog HTML component

This library provides the SWISH page  and   its  elements as Prolog HTML
grammer rules. This allows for server-side   generated  pages to include
swish or parts of swish easily into a page.
*/

http:location(pldoc, swish(pldoc), [priority(100)]).

:- http_handler(swish(.), swish_reply([]), [id(swish), prefix]).

:- multifile
	swish_config:logo//1,
	swish_config:source_alias/2,
	swish_config:reply_page/1,
	swish_config:li_login_button//1.

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
%	  - show_beware(Boolean)
%	  Control showing the _beware limited edition_ warning.
%	  - preserve_state(Boolean)
%	  If `true`, save state on unload and restore old state on load.

swish_reply(Options, Request) :-
	(   option(identity(_), Options)
	->  Options2 = Options
	;   authenticate(Request, Auth),
	    Options2 = [identity(Auth)|Options]
	),
	swish_reply2(Options2, Request).

swish_reply2(Options, Request) :-
	option(method(Method), Request),
	Method \== get, Method \== head, !,
	swish_rest_reply(Method, Request, Options).
swish_reply2(_, Request) :-
	swish_reply_resource(Request), !.
swish_reply2(Options, Request) :-
	swish_reply_config(Request, Options), !.
swish_reply2(SwishOptions, Request) :-
	Params = [ code(_,	  [optional(true)]),
		   show_beware(_, [optional(true)]),
		   background(_,  [optional(true)]),
		   examples(_,    [optional(true)]),
		   q(_,           [optional(true)]),
		   format(_,      [oneof([swish,raw,json]), default(swish)])
		 ],
	http_parameters(Request, Params),
	params_options(Params, Options0),
	add_show_beware(Options0, Options1),
	add_preserve_state(Options1, Options2),
	merge_options(Options2, SwishOptions, Options3),
	source_option(Request, Options3, Options4),
	option(format(Format), Options4),
	swish_reply3(Format, Options4).

swish_reply3(raw, Options) :-
	option(code(Code), Options), !,
	format('Content-type: text/x-prolog~n~n'),
	format('~s', [Code]).
swish_reply3(json, Options) :-
	option(code(Code), Options), !,
	option(meta(Meta), Options, _{}),
	option(chat_count(Count), Options, 0),
	reply_json_dict(json{data:Code, meta:Meta, chats:_{total:Count}}).
swish_reply3(_, Options) :-
	swish_config:reply_page(Options), !.
swish_reply3(_, Options) :-
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

%!	add_show_beware(+Options0, -Option) is det.
%
%	Add show_beware(false) when called with code, query or examples.
%	These are dedicated calls that do not justify this message.

add_show_beware(Options0, Options) :-
	implicit_no_show_beware(Options0), !,
	Options = [show_beware(false)|Options0].
add_show_beware(Options, Options).

implicit_no_show_beware(Options) :-
	option(show_beware(_), Options), !,
	fail.
implicit_no_show_beware(Options) :-
	\+ option(format(swish), Options), !,
	fail.
implicit_no_show_beware(Options) :-
	option(code(_), Options).
implicit_no_show_beware(Options) :-
	option(q(_), Options).
implicit_no_show_beware(Options) :-
	option(examples(_), Options).
implicit_no_show_beware(Options) :-
	option(background(_), Options).

%!	add_preserve_state(+Options0, -Option) is det.
%
%	Add preserve_state(false) when called with code.

add_preserve_state(Options0, Options) :-
	option(preserve_state(_), Options0), !,
	Options = Options0.
add_preserve_state(Options0, Options) :-
	option(code(_), Options0), !,
	Options = [preserve_state(false)|Options0].
add_preserve_state(Options, Options).


%%	source_option(+Request, +Options0, -Options)
%
%	If the data was requested  as   '/Alias/File',  reply using file
%	Alias(File).

source_option(_Request, Options0, Options) :-
	option(code(Code), Options0),
	option(format(swish), Options0), !,
	(   uri_is_global(Code)
	->  Options = [url(Code),st_type(external)|Options0]
	;   Options = Options0
	).
source_option(Request, Options0, Options) :-
	source_file(Request, File, Options0), !,
	option(path(Path), Request),
	(   source_data(File, String, Options1)
	->  append([ [code(String), url(Path), st_type(filesys)],
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
	PathInfo \== 'index.html',
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

source_data(Path, Code, [title(Title), type(Ext), meta(Meta)]) :-
	setup_call_cleanup(
	    open(Path, read, In, [encoding(utf8)]),
	    read_string(In, _, Code),
	    close(In)),
	source_metadata(Path, Code, Meta),
	file_base_name(Path, File),
	file_name_extension(Title, Ext, File).

%%	source_metadata(+Path, +Code, -Meta:dict) is det.
%
%	Obtain meta information about a local  source file. Defined meta
%	info is:
%
%	  - last_modified:Time
%	  Last modified stamp of the file.  Always present.
%	  - loaded:true
%	  Present of the file is a loaded source file
%	  - modified_since_loaded:true
%	  Present if the file loaded, has been edited, but not
%	  yet reloaded.

source_metadata(Path, Code, Meta) :-
	findall(Name-Value, source_metadata(Path, Code, Name, Value), Pairs),
	dict_pairs(Meta, meta, Pairs).

source_metadata(Path, _Code, path, Path).
source_metadata(Path, _Code, last_modified, Modified) :-
	time_file(Path, Modified).
source_metadata(Path, _Code, loaded, true) :-
	source_file(Path).
source_metadata(Path, _Code, modified_since_loaded, true) :-
	source_file_property(Path, modified(ModifiedWhenLoaded)),
	time_file(Path, Modified),
	ModifiedWhenLoaded \== Modified.
source_metadata(Path, _Code, module, Module) :-
	file_name_extension(_, Ext, Path),
	user:prolog_file_type(Ext, prolog),
	xref_public_list(Path, _, [module(Module)]).

confirm_access(Path, Options) :-
	option(if(Condition), Options), !,
	must_be(oneof([loaded]), Condition),
	eval_condition(Condition, Path).
confirm_access(_, _).

eval_condition(loaded, Path) :-
	source_file(Path).

%%	swish_reply_resource(+Request) is semidet.
%
%	Serve /swish/Resource files.

swish_reply_resource(Request) :-
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
		       [ ul([class([nav, 'navbar-nav', menubar])], []),
			 ul([class([nav, 'navbar-nav', 'navbar-right'])],
			    [ li(\notifications(Options)),
			      li(\search_box(Options)),
			      \li_login_button(Options),
			      li(\broadcast_bell(Options))
			    ])
		       ])
		 ])).

li_login_button(Options) -->
	swish_config:li_login_button(Options).
li_login_button(_Options) -->
	[].

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
	swish_config:logo(Options), !.
swish_logos(Options) -->
	pengine_logo(Options),
	swish_logo(Options).

%!	swish_config:logo(+Options)// is semidet.
%
%	Hook  to  include  the  top-left    logos.   The  default  calls
%	pengine_logo//1 and swish_logo//1.  The   implementation  should
%	emit zero or more <a> elements.

%!	pengine_logo(+Options)// is det.
%!	swish_logo(+Options)// is det.
%
%	Emit an <a> element that provides a   link to Pengines and SWISH
%	on this server. These may be called from swish_config:logo//1 to
%	include the default logos.

pengine_logo(_Options) -->
	{ http_absolute_location(root(.), HREF, [])
	},
	html(a([href(HREF), class('pengine-logo')], &(nbsp))).
swish_logo(_Options) -->
	{ http_absolute_location(swish(.), HREF, [])
	},
	html(a([href(HREF), class('swish-logo')], &(nbsp))).


%%	swish_content(+Options)//
%
%	Generate the SWISH editor, Prolog output  area and query editor.
%	Options processed:
%
%	  - source(HREF)
%	  Load initial source from HREF
%	  - chat_count(Count)
%	  Indicate the presense of Count chat messages

swish_content(Options) -->
	{ document_type(Type, Options)
	},
	swish_resources,
	swish_config_hash(Options),
	swish_options(Options),
	html(div([id(content), class([container, 'tile-top'])],
		 [ div([class([tile, horizontal]), 'data-split'('50%')],
		       [ div([ class([editors, tabbed])
			     ],
			     [ \source(Type, Options),
			       \notebooks(Type, Options)
			     ]),
			 div([class([tile, vertical]), 'data-split'('70%')],
			     [ div(class('prolog-runners'), []),
			       div(class('prolog-query'), \query(Options))
			     ])
		       ]),
		   \background(Options),
		   \examples(Options)
		 ])).


%%	swish_config_hash(+Options)//
%
%	Set `window.swish.config_hash` to a  hash   that  represents the
%	current configuration. This is used by   config.js  to cache the
%	configuration in the browser's local store.

swish_config_hash(Options) -->
	{ swish_config_hash(Hash, Options) },
	js_script({|javascript(Hash)||
		   window.swish = window.swish||{};
		   window.swish.config_hash = Hash;
		   |}).


%!	swish_options(+Options)//
%
%	Emit additional options. This is  similar   to  config,  but the
%	config object is big and stable   for a particular SWISH server.
%	The options are set per session.

swish_options(Options) -->
	js_script({|javascript||
		   window.swish = window.swish||{};
		   window.swish.option = window.swish.option||{};
		  |}),
	swish_options([show_beware, preserve_state], Options).

swish_options([], _) --> [].
swish_options([H|T], Options) -->
	swish_option(H, Options),
	swish_options(T, Options).

swish_option(Name, Options) -->
	{ Opt =.. [Name,Val],
	  option(Opt, Options),
	  JSVal = @(Val)
	}, !,
	js_script({|javascript(Name, JSVal)||
		   window.swish.option[Name] = JSVal;
		   |}).
swish_option(_, _) -->
	[].

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
%	  - url(+URL)
%	  as file(File), but used if the data is loaded from an
%	  alias/file path.
%	  - title(+Title)
%	  Defines the title used for the tab.

source(pl, Options) -->
	{ option(code(Spec), Options), !,
	  download_source(Spec, Source, Options),
	  phrase(source_data_attrs(Options), Extra)
	},
	html(div([ class(['prolog-editor']),
		   'data-label'('Program')
		 ],
		 [ textarea([ class([source,prolog]),
			      style('display:none')
			    | Extra
			    ],
			    Source)
		 ])).
source(_, _) --> [].

source_data_attrs(Options) -->
	(source_file_data(Options) -> [] ; []),
	(source_url_data(Options) -> [] ; []),
	(source_title_data(Options) -> [] ; []),
	(source_meta_data(Options) -> [] ; []),
	(source_st_type_data(Options) -> [] ; []),
	(source_chat_data(Options) -> [] ; []).

source_file_data(Options) -->
	{ option(file(File), Options) },
	['data-file'(File)].
source_url_data(Options) -->
	{ option(url(URL), Options) },
	['data-url'(URL)].
source_title_data(Options) -->
	{ option(title(File), Options) },
	['data-title'(File)].
source_st_type_data(Options) -->
	{ option(st_type(Type), Options) },
	['data-st_type'(Type)].
source_meta_data(Options) -->
	{ option(meta(Meta), Options), !,
	  atom_json_dict(Text, Meta, [])
	},
	['data-meta'(Text)].
source_chat_data(Options) -->
	{ option(chat_count(Count), Options),
	  atom_json_term(JSON, _{count:Count}, [as(string)])
	},
	['data-chats'(JSON)].

%%	background(+Options)//
%
%	Associate  the  background  program  (if  any).  The  background
%	program is not displayed in  the  editor,   but  is  sent to the
%	pengine for execution.

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
		       NoteBookText)
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
%	@arg Type is one of `swinb` or `pl`

document_type(Type, Options) :-
	(   option(type(Type0), Options)
	->  Type = Type0
	;   option(meta(Meta), Options),
	    file_name_extension(_, Type0, Meta.name),
	    Type0 \== ''
	->  Type = Type0
	;   option(st_type(external), Options),
	    option(url(URL), Options),
	    file_name_extension(_, Ext, URL),
	    ext_type(Ext, Type)
	->  true
	;   Type = pl
	).

ext_type(swinb, swinb).


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
	read_data(Type, Request, Data, Meta),
	authorized(file(update(File,Meta)), Options1),
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
