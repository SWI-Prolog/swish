:- module(swish_elastic, []).
:- use_module(library(chat80)).

/** <module> Use Elastic Search for finding documents.

This config file enables using Elastic Search  for indexing the files in
our database. Small installations can  easily   deal  with native Prolog
search. For e.g., swish.swi-prolog.org this   becomes  too expensive. We
could load most of the (meta) data   into Prolog, but this increases the
startup time and memory footprint of SWISH.
*/

:- use_module(swish(lib/plugin/es_swish)).

:- multifile elastic:es_server/2.
:- multifile swish_config:config/2.
swish_config:config(sourcelist,
                    #{ engine: elastic,
                       help: 'es_sourcelist.html'
                     }).

%!  elastic:es_server(-URL, -Options) is det.
%
%   Elastic  search  server.  When  TLS  is   configured  we  must  pass
%   authorization and certificate options.  If   we  used  a self-signed
%   certificate, use the `cacerts`  option   to  specify the certificate
%   authority  and  use  `cert_verify_hook(tls_verify)`  to  ignore  the
%   hostname when matching the certificate.

elastic:es_server('http://localhost:9200',
                  [ authorization(basic(elastic,"<password>")),
                    cacerts([file('config/certs/http_ca.crt')]),
                    cert_verify_hook(tls_verify)
                  ]).

