# SWISH TODO list

## Infrastructure

  - How/where to distribute the built files?  Generate a zip?  Distinct
    git?

## Design

  - Modularize navbar population?

## Window handling

  - Add/remove new windows to the tile (pane.js can do that).
  - Specify more about sized: minimum size, flexibility.  Probably
    requires extending jquery.splitter.
  - Allow moving panes around using drag/drop.  Allow organizing
    in tabs?

## Query editor

  - Quickly create a query from a predicate?
    - Menu from source editor based on cursor position
      - If in head, create goal from current predicate
        - Can we do something with the arguments?
      - If in goal, copy goal to query window

## Runners

  - Get back to the associated source version (open new editor tab?).
  - Find source changes compared to current version (easy)

## Debugger

  - Show stack and choice-points?
  - Display constraints.  How?

## Rendering framework

  - User provided rendering?
    - Allow for rewriting answer terms?
    - Allow generating HTML?  How to deal with security?

## Sharing

First option was TogetherJS.  Now doubting, as it my be much better to
_not_ see exactly the same UI for cooperation.  What about

  - Allow for sharing editors
  - Allow sharing runners (= query+program)
  - Provide chat and whiteboard (Anne)

## Saving files

  - Look at [Dillinger markup editor](http://dillinger.io/)
    - Save to github
  - Save/Info dialogs
    - Fork from history
      - Now: Play, Save, clear/fill name.
      - New: Fork button?
        - Will do
	  - Load new data
	  - Save, clearing name.
    - Indicate branch points in history?
      - Requires complete graph in memory.
  - Re-map Control-S (google-docs ignores save)

## Login

  - Social login
    - login-with-google (oauth2)
    - etc ...
  - Make it easy to add HTTPS certificate based login.
    - not SWISH specific

## Search

  - Implement string search with result page
    - `Harmonica' with
      - Found files (including full text search inside them)
      - Found manual entries
      - Found source locations

## Tabbed editor

  - Source search: pass number of items being searched and
    if there are too many hits, balance over files.
  - Set tab-width per source?
  - Debug (trace) through included files
    - Works, except for following the source.
  - Deal with files/line numbers over multiple files
  - Staging
    Control-S (whatever) saves data to `staging area'.
    - In addition to HEAD, introduce STAGE (gitty solution)?
    - Save to browser store?
      - Retrieve on reload?
      - Make sure running includes the browser version.
        - Send list of modified tabs

## Notebooks

  - Save notebook/program as new profile?
  - Collapsible sections?
  - Move sections (or select multiple)
  - Delete query from menu?  Better: undo for delete.

## SWISH as Prolog frontend for local usage

  - Deal with login
    - Limit to localhost.  Proposed by Douglas Miles:
      - Generate random initial URL
      - Fire on this URL
      - Establish session cookie
      - Demand this cookie and destroy the initial URL
    - Shared/remote usage
      - Describe how to setup HTTPS.
  - Improve source search
    - Full search
    - Search file names
    - Regex support
    - Select search targets
      - File names
      - Content
      - Gitty, filesys, examples
  - Debugger:
    - Set breakpoints in non-pengine sources
      - For shared operation this requires limiting a breakpoint to
        the pengine thread.
    - Get a stack overview.
  - Mark files as pengine_src, loaded, not_loaded
    - Only send pengine_src with pengines.
    - Detect pengine_src based on alias?

### Bugs

  - Colouring of /library/http/html_write.pl.  Broken sequences:
    - DCG exports
    - ?- [file].

