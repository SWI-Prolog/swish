/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016, VU University Amsterdam

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
