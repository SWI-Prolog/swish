# SWISH TODO list

## Infrastructure

  - How/where to distribute the built files?  Generate a zip?  Distinct
    git?

## Design

  - Modularize navbar population?

## Window handling

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

## Dashboard

Dashboard entry is basically a query. We will specify the parameters as

```
paramters(
    [ Param1: Option ("+" Option)*
      ...
    ]),

Goal
```

parameters/1 creates a dialog from the  provided argument types. We will
do that server side. Initially we will  use the Pengine _prompt_ to fill
the dialog, so parameters/1 acts as  read/1.   In  later versions we can
provide a setting to the query that   will perform the generation of the
dialog at notebook load time  and  replace   the  query  by  its dialog.
Possibly we can implement that as part of the server-side highlighting?


## Sharing

First option was TogetherJS.  Now doubting, as it my be much better to
_not_ see exactly the same UI for cooperation.  What about

  - Allow for sharing editors
  - Allow sharing runners (= query+program)

## File ownership and rights

  - If a user is logged in
    - Store the profile-id with a saved program			[OK]
    - Do not show E-mail					[OK]
    - Provide options:
      - Allow save a new version
        - Anyone						[OK]
	- Logged on						[OK]
	- Team
	- Only me						[OK]
      - Set/unset follow (docid, profile-id)
        - When saving a file					[OK]
	- From File/Follow menu					[OK]
      - Vote up/down
      - Profile options for email				[OK]
    - Email notifications
      - Exclude sending mail to self `self'			[OK]
      - Provide `stop following this file' link			[OK]
      - Provide `stop sending email' link			[OK]
      - Styling						        [Not yet pretty]

## Make teams

  - A team has a homepage, which is a notebook.
  - Workflow:
    - Save a copy of the "Team homepage" example notebook.
    - Turn into team homepage
      - Add "team" tag
      - Have a members query, returning a table.  Using an
        HTML cell?
      - Create as a copy from an example page?
    - Invite team members (is this a general page behaviour)
      - Input with look-ahead for searching people (or type
	email).
      - Text area for invitation
      - Sends e-mail.
  - Thus a team is
    - A page
    - Link to members
  - Small installations can automatically add all users to the
    one team.


## Saving files

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

