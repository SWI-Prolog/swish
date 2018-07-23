:- module(swish_config_chat80, []).
:- use_module(library(chat80)).

/** <module> Make CHAT80 demo available

This config file makes  the  CHAT80   program  available  from SWISH. It
declares the toplevel predicates of the program safe. The CHAT80 program
itself is available from the link  below   and  may be attached to SWISH
during its configuration.

@see https://github.com/JanWielemaker/chat80
*/

:- multifile
    sandbox:safe_primitive/1.

sandbox:safe_primitive(chat80:chat_process(_,_)).
sandbox:safe_primitive(chat80:chat_parse(_,_)).
sandbox:safe_primitive(chat80:chat_semantics(_,_)).
