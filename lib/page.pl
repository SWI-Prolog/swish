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
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_path)).
:- if(exists_source(library(http/http_ssl_plugin))).
:- use_module(library(http/http_ssl_plugin)).
:- endif.
:- use_module(library(debug)).
:- use_module(library(time)).
:- use_module(library(option)).
:- use_module(config).

/** <module> Provide the SWISH application as Prolog HTML component

This library provides the SWISH page  and   its  elements as Prolog HTML
grammer rules. This allows for server-side   generated  pages to include
swish or parts of swish easily into a page.
*/

:- http_handler(swish(.), swish_reply([]), [id(swish), prefix]).

:- multifile
	swish_config:source_alias/1,
	swish_config:reply_page/1.

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

swish_reply(_, Request) :-
	serve_resource(Request), !.
swish_reply(_, Request) :-
	swish_reply_config(Request), !.
swish_reply(SwishOptions, Request) :-
	Params = [ code(_,	 [optional(true)]),
		   background(_, [optional(true)]),
		   examples(_,   [optional(true)]),
		   q(_,          [optional(true)])
		 ],
	http_parameters(Request, Params),
	params_options(Params, Options0),
	merge_options(Options0, SwishOptions, Options1),
	source_option(Request, Options1, Options),
	(   swish_config:reply_page(Options)
	->  true
	;   reply_html_page(
		swish(main),
		[ title('SWISH -- SWI-Prolog for SHaring'),
		  link([ rel('shortcut icon'),
			 href('/icons/favicon.ico')
		       ]),
		  link([ rel('apple-touch-icon'),
			 href('/icons/swish-touch-icon.png')
		       ])
		],
		\swish_page(Options))
	).

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

source_option(_Request, Options, Options) :-
	option(code(_), Options), !.
source_option(Request, Options0, Options) :-
	option(path_info(Info), Request),
	Info \== 'index.html', !,	% Backward compatibility
	(   source_data(Info, String)
	->  Options = [code(String)|Options0]
	;   http_404([], Request)
	).
source_option(_, Options, Options).

source_data(Info, Code) :-
	sub_atom(Info, B, _, A, /),
	sub_atom(Info, 0, B, _, Alias),
	sub_atom(Info, _, A, 0, File),
	catch(swish_config:source_alias(Alias), E,
	      (print_message(warning, E), fail)),
	Spec =.. [Alias,File],
	http_safe_file(Spec, []),
	absolute_file_name(Spec, Path,
			   [ access(read),
			     file_errors(fail)
			   ]),
	setup_call_cleanup(
	    open(Path, read, In, [encoding(utf8)]),
	    read_string(In, _, Code),
	    close(In)).

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

swish_navbar(_Options) -->
	swish_resources,
	html(header(class([navbar, 'navbar-default']),
		    div(class([container, 'pull-left']),
			[ div(class('navbar-header'),
			      \swish_logos),
			  nav(id(navbar), [])
			]))).

swish_logos -->
	pengine_logo,
	swish_logo.

pengine_logo -->
	{ http_absolute_location(root(.), HREF, [])
	},
	html(a([href(HREF), class('pengine-logo')], &(nbsp))).
swish_logo -->
	{ http_absolute_location(swish('index.html'), HREF, [])
	},
	html(a([href(HREF), class('swish-logo')], &(nbsp))).

%%	swish_content(+Options)//
%
%	Generate the SWISH editor, Prolog output  area and query editor.
%	Options processed:
%
%	  - source(HREF)
%	  Load initial source from HREF

swish_content(Options) -->
	swish_resources,
	html(div([id(content), class([container, swish])],
		 [ div([class([tile, horizontal]), 'data-split'('50%')],
		       [ div(class('prolog-editor'), \source(Options)),
			 div([class([tile, vertical]), 'data-split'('70%')],
			     [ div(class('prolog-runners'), []),
			       div(class('prolog-query'), \query(Options))
			     ])
		       ]),
		   \background(Options),
		   \examples(Options)
		 ])).

%%	source(+Options)//
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

source(Options) -->
	{ option(code(Spec), Options), !,
	  download_source(Spec, Source, Options),
	  (   option(file(File), Options)
	  ->  Extra = ['data-file'(File)]
	  ;   Extra = []
	  )
	},
	html(textarea([ class([source,prolog]),
			style('display:none')
		      | Extra
		      ],
		      Source)).
source(_) --> [].


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


%%	download_source(+HREF, -Source, Options) is det.
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
				[ cert_verify_hook(ssl_verify)
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
		   '%ERROR: Content too long (max ~D)~n', [MaxLen])
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
		   '%ERROR: Content too long (max ~D)~n', [MaxLen])
	).

load_error(E, Source) :-
	message_to_string(E, String),
	format(string(Source), '%ERROR: ~s~n', [String]).

:- public ssl_verify/5.

%%	ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%	Currently we accept  all  certificates.   We  organise  our  own
%	security using SHA1 signatures, so  we   do  not  care about the
%	source of the data.

ssl_verify(_SSL,
	   _ProblemCertificate, _AllCertificates, _FirstCertificate,
	   _Error).


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

