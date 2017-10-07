:- module(config_http_auth, []).
:- use_module(swish(lib/config), []).

/** <module> Optional HTTP based authentication

Enable optional HTTP based authentication.

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
swish_config:config(public_access, true).

:- use_module(swish(lib/plugin/http_authenticate), []).
% Make adding users available from the toplevel
:- use_module(user:swish(lib/plugin/http_authenticate),
              [ swish_add_user/3,	% +User, +Passwd, +Fields
                swish_add_user/1,	% +Dict
                swish_add_user/0
              ]).

% Can be set to `basic` when HTTPS is used.  Using `basic` saves
% one round trip but requires HTTPS to avoid exchanging the password
% unencrypted.
% :- set_setting_default(swish_authenticate:method, basic).
