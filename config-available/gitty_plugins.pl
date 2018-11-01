:- module(config_gitty_plugin, []).
:- use_module(swish(lib/storage)).

/** <module> Use gitty versioned files as plugins

Normally files in the SWISH  store  are   only  loaded  into a temporary
module for running a single query. This   config file allows for loading
files from the SWISH store as normal modules.

This can be used to  make   arbitrary,  globally visible, and persistent
changes to the server. It is typically  used to maintain shared code and
perform configuration in SWISH servers that  are preloaded with programs
and data and are used by a relatively small user population.

A file that is loaded using use_gitty_file/1   is loaded in the same way
as load_files/2. Note that, unlike normal   files  from the store, files
loaded using use_gitty_file/1 are  normally   *module  files*. After the
file is saved from the SWISH interface it is automatically reloaded.

The following workflow is recommended:

  1. Create a file in the SWISH interface, say `myfile.pl`.
  2. Save it while logged in and set the permissions appropriately.
     *|Being able to save the file gives complete control over the
     server!|*
  3. Add a `:- use_gitty(myfile)` below.
  4. Either load this config file by hand from the SWISH query window
     or restart the server.

Note that by loading  the  module   prefixed  by  =|swish:|=, its public
predicates are available for  all  queries.   Most  plugin  files define
hooks. Such files should be defined  as   a  module with an empty export
list.
*/


% :- use_gitty_file(myfile).
% :- use_gitty_file(swish:myfile).
