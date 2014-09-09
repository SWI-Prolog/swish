# Server enriched highlighting

## Re-synchronization

The server and client code can become out-of-sync due to editing. We can
see the following cases:

  * Edit starts a new term.

  * Edit modifies a term.
    * Edit creates a syntax error


## Syntax errors

If there is a syntax error, client finds tokens, but server only emits
"syntax_error" for the whole term.
