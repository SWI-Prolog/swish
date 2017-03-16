# SWISH configuration

At startup, SWISH reads *.pl files  from the directory `config-enabled`.
The *.pl are loaded using ensure_loaded/1 in alphabetical order.

The directory `config-available` provides  skeleton   files  for  common
configuration settings. These files may  be   linked  from  or copied to
`config-enabled`.

## Configure authentication

The following authentication scenarios may be configured

  - Without any authentication enabled, SWISH can be accessed without
    logging in and can only be used to execute _safe_ goals.

  - Using =auth_http_always.pl=, any access to SWISH requires
    authentication.  This config module is not compatible with one
    of the other auth_* configuration modules.

  - Load one or more of the auth_*.pl modules. This provides a *Login*
    button in the navigation bar.  Note that the authentication
    modules require additional configuration.


## Resource limitation

  - `rlimit.pl` supports limiting the memory (and optionally other
    resources) that can be used by the SWISH server.


## Configure collaboration

A number of configuration modules contribute to enabling collaboration
support such as managing users and get notifications.

  - `email.pl` needs to be installed to enable notifications using
    mail.  This module needs to be configured for contacting a mail
    gateway. Email also requires the GIT submodule pack/smtp to be
    checked out.

  - `notifications.pl` and `user_profile.pl` need to be enabled to
    send notification and manage user profiles.  The latter also
    requires the GIT submodule pack/profile to be checked out.

  - `network.pl` is often needed to tell swish about the public
    HTTP host and port at which it is accessible.  This is notably
    the case if proxies or tunnels are involved.

## R integration

  - The `r_serve.pl` config file provides R integration.  See
    `r_serve.pl` for how to setup R itself for use with SWISH.
    R also requires the GIT submodule pack/rserve_client to be
    checked out and configured using

	```
	?- pack_rebuild(rserve_client).
	```

## Debugging and logging.

  - `debug.pl` provides several hints and tools to simplify
    server-side debugging of SWISH.

  - `logging.pl` configures the creation of HTTP and Pengine log
    files.

