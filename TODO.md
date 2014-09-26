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

## Editor

  - Add server extensions to CodeMirror
  - Advanced completion.  This seems possible.  See e.g.,
    - https://groups.google.com/forum/#!topic/codemirror/9OvD5cMz_uA
    - can we do menu-based completions?

## Query handling

  - Fire query when complete on <RETURN>
  - Provide next/stop interaction using the keyboard.
  - Provide a next-N (10, 100)

## Runners

  - Allow to move them into toplevel browser windows?  Is that possible?
  - Get back to the associated source version (open new editor tab?).
    Fork/re-run.

## Answers

  - We must distinguish two types of queries:

    - Relational queries that provide simple data per variable.  Such
      queries require a table as output at the option to do next 10, 100, ...
    - Queries that produce a complex datastructure.  E.g., a parse-tree,
      a matrix, etc.  Here, just `next' is ok, and we need a pluggable
      rendering framework.

## Sharing

First option was TogetherJS.  Now doubting, as it my be much better to
_not_ see exactly the same UI for cooperation.  What about

  - Allow for sharing editors
  - Allow sharing runners (= query+program)
  - Provide chat and whiteboard (Anne)

## Saving files

  - Look at [Dillinger markup editor](http://dillinger.io/)
  - Server saves:

    - Save under content hash?  Would mean that `Back' actually goes back
      a version!  Provide meta-information (who, previous).
    - Use GIT?  We have most of that!

  - Get rid of # urls.  See
    http://stackoverflow.com/questions/824349/modify-the-url-without-reloading-the-page
