/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(ssl_certificate,
	  [
	  ]).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(socket)).
:- use_module(library(filesex)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

/** <module> Generate SSL certificates

This library simplifies the task of running SWISH using HTTPS, so we can
provide secure authentication and private communication.   It  acts as a
hook  into  library(http/http_unix_daemon),  creating    a   self-signed
certificate if the daemon is started with the option =|--https|=.

The CN (Common Name), O (Organization) and   C  (Country) options are by
default derived from the network configuration and http://ipinfo.io/. If
you wish to provide your own, use the following options:

  - --CN=<host>
  - --O=<organization>
  - --C=<country code>

For example:

  ```
  ./daemon --https --CN=myhost.org --O="Me and my dog" --C=UK'
  ```

@tbd	Support Let's Encrypt (https://letsencrypt.org/)
*/

%%	http_unix_daemon:http_certificate_hook(+CertFile, +KeyFile, -Password)
%
%	Generate  a  self-signed  certificate  if    no  certificate  is
%	provided.

:- multifile http_unix_daemon:http_certificate_hook/3.

http_unix_daemon:http_certificate_hook(CertFile, KeyFile, _) :-
	(   exists_file(CertFile)
	;   exists_file(KeyFile)
	), !,
	print_message(informational, ssl_certificate(CertFile, KeyFile)).
http_unix_daemon:http_certificate_hook(CertFile, KeyFile, _) :-
	cert_subject(Subject),
	print_message(informational, ssl_self_signed(CertFile, KeyFile, Subject)),
	ensure_parent_dir(CertFile),
	ensure_parent_dir(KeyFile),
	process_create(path(openssl),
		       [ req,
			 '-subj', Subject,
			 '-new', '-newkey', 'rsa:2048',
			 '-days', '365',
			 '-nodes', '-x509',
			 '-keyout', file(KeyFile), '-out', file(CertFile)
		       ], []).

ensure_parent_dir(File) :-
	file_directory_name(File, Dir),
	make_directory_path(Dir).

cert_subject(Subject) :-
	cert_host(Host),
	cert_org_country(Host, Org, Country),
	format(atom(Subject), '/CN=~w/O=~w/C=~w', [ Host, Org, Country ]).

cert_host(Host) :- argv_param('CN', Host), !.
cert_host(Host) :- gethostname(Host).

cert_org_country(_Host, Org, Country) :-
	argv_param('O', Org),
	argv_param('C', Country), !.
cert_org_country(Host, Org, Country) :-
	catch(host_info(Host, Dict), E,
	      ( print_message(warning, E),
		fail)),
	\+ Dict.get(bogon) == 1,
	Country0 = Dict.get(country),
	Org0 = Dict.get(org), !,
	argv_param('O', Org, Org0),
	argv_param('C', Country, Country0).
cert_org_country(_Host, 'SWISH', 'ZZ') :-
	print_message(warning, ssl_no_cert_subject).

argv_param(Param, Value, Default) :-
	(   argv_param(Param, Value0)
	->  Value = Value0
	;   Value = Default
	).

argv_param(Param, Value) :-
	current_prolog_flag(argv, Argv),
	atomic_list_concat(['--', Param, =], Prefix),
	member(Arg, Argv),
	atom_concat(Prefix, Value0, Arg),
	(   atom_number(Value0, Number)
	->  Value = Number
	;   Value = Value0
	).

host_info(Host, Dict) :-
	tcp_host_to_address(Host, ip(A,B,C,D)),
	format(string(URL), 'http://ipinfo.io/~w.~w.~w.~w', [A,B,C,D]),
	http_open(URL, In,
		  [ request_header('Accept'='application/json')
		  ]),
	call_cleanup(json_read_dict(In, Dict),
		     close(In)).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(ssl_self_signed(CertFile, _KeyFile, Subject)) -->
	[ 'Creating self-signed certificate in ~p'-[CertFile], nl,
	  'Certificate subject: "~w"'-[Subject]
	].
prolog:message(ssl_certificate(CertFile, KeyFile)) -->
	[ 'Using SSL certificate from ~p, key from ~p'-[CertFile, KeyFile] ].
prolog:message(ssl_no_cert_subject) -->
	[ 'Failed to find a fully qualified hostname, organization and country.'-[], nl,
	  'Using dummy values.  If you want proper values:'-[], nl,
	  '  - Remove the generated certificates from the https directory'-[], nl,
	  '  - Re-run using the options --CN=host --O=organization --C=country'-[], nl,
	  '    e.g. ./daemon --https --CN=myhost.org --O="Me and my dog" --C=UK'-[]
	].
