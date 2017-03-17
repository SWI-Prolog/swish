:- module(config_auth_http_always, []).
:- use_module(swish(lib/config), []).

/** <module> Exclusive HTTP based authentication

Enable HTTP based authentication for any  access   to  SWISH. If this is
enabled, none of the other auth_* modules should be enabled.

Note that lib/authenticate.pl  supports  both   HTTP  _basic_  and  HTTP
_digest_ login. Please make  sure  to   understand  the  security issues
involved with using authenticated access.  In a nutshell:

  - *Basic* authentication should only be used together with HTTPS as
    it exposes the user and password on the wire.
  - *Digest* authentication uses a challenge/response protocol to avoid
    exposing the password and a _nonce_ to avoid steeling a connection
    by reusing credentials.  The rest of the communication is insecure.
*/

:- multifile swish_config:config/2.
swish_config:config(public_access, false).

:- use_module(swish(lib/plugin/http_authenticate), []).
:- use_module(swish(lib/plugin/login), []).

% Can be set to `basic` when HTTPS is used.  Using `basic` saves
% one round trip but requires HTTPS to avoid exchanging the password
% unencrypted.
% :- set_setting_default(swish_authenticate:method, basic).
