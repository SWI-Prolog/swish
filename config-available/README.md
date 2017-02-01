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
    button in the navigation bar.  Note that many of the authentication
    modules require additional configuration.
