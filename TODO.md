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

  - Template based completion					[OK]
  - Indicate changes to last saved point			[OK]
  - Download menu?

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

  - Core from Prolog ok.
  - Need data vizualizations.  What is a good framework?
    - Settled for C3.js, which also brings D3.js into the picture.
      - :- use_rendering(c3).					[OK]
        - Data representation is not always natural from Prolog.
	  Need to recognise and map common representations.  E.g.,
          - Pairs to columns for pie and bar charts.
	  - Recognise textual labels and set the axis accordingly

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

  - swish.playFile()
    - If current tab can accept type, play it there		[OK]
    - If current tab is a `new tab', play it there		[OK]
    - else, add a new tab					[OK]
    - If file is already opened in a tab, switch to that tab.
  - Add notion of `current source' to be used by the		[OK]
    bottom-right query editor.  This is:
    - Visible Prolog editor
    - If in a notebook, the active one.
    - Populate the query example window from there
  - Run Markdown cells when they become inactive		[OK]
  - Notebook
    - Connect query to above source.				[OK]
    - Multiple sources:
      - Allow naming sources?
      - Allow one source to include others in the same notebook?
    - Allow a notebook to make SWISH specific links:
      [Nice example to say hello](hello.swinb)			[OK]
      [Actions are defined here](actions.pl)			[OK]
    - Avoid hyperlinks to destroy the page.
      - show predicate links in a modal dialog
        - /pldoc/man?predicate=member/2				[OK]
	- other predicate links.
      - Use `target=` for others.
  - Use type icons for tabs and hide the file extension.	[OK]
  - Use type icons for examples menu and search results.	[OK]
  - Use swish('playFile', ...) for the back button.		[OK]
  - Add close-button to runner without title.			[OK]
  - Add help screen for empty notebook.				[OK]
  - Check modified status of tab before closing it.		[OK]
  - Source search: pass number of items being searched and
    if there are too many hits, balance over files.
  - Source search: add type icon to file headers.		[OK]
  - Limit search results to configured public directories.	[OK]
  - Set tab-width per source?

### Bugs

  - Colouring of /library/http/html_write.pl
    - DCG exports broken?
    - use_module/2 broken.
