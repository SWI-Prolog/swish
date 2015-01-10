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

## Runners

  - Allow to move them into toplevel browser windows?  Is that possible?
  - Get back to the associated source version (open new editor tab?).
    Fork/re-run.

## Rendering framework

  - Core from Prolog ok.
  - Need data vizualizations.  What is a good framework?  There are some
    overviews [1,2,3]. Some tools:

    - d3.js
      Very capable, but we need something that does not need configuration,
      only data and selection of the vizualization type.
    - c3.js
      Might give pure data, but looks really limited.  Flot might be
      more lightweight in that case.
    - NVD3
      Similar to c3.js.  Looks a bit more promising, but also seems
      to require more input.
    - flot
      Nice and simple, but can only do simple graphs and piecharts.
    - Flotr2
      Might be better than flot.
    - http://ushiroad.com/jsviz/
      Graphviz alternative in pure JS.
    - Dagre (https://github.com/cpettitt/dagre)
      Provides directed graph layout.  Agnostic of shapes.  Can be used
      together with JointJs.
    - http://www.jointjs.com
      General interactive diagramming and charting tool.  Provides layout
      with Dagre plugin.  Seems weak on tree-like structures.
    - jQuery Sparklines
      Lightweight and simple, but only designed for small graphs using small
      datasets.  Might be nice to show load/stack/CPU, etc.
  - Special purpose needs:
    - Something like gvterm?
    - Parse trees
    - Geo Maps: Leaflet (Carlo Capelli).

[1] http://datavisualization.ch/tools/13-javascript-libraries-for-visualizations/
[2] http://jster.net/category/visualization-libraries
[3] http://blog.profitbricks.com/39-data-visualization-tools-for-big-data/

### Some data vizualization literature:

 - "Reviewing data visualization: an analytical taxonomical study"
   JF Rodrigues, AJM Traina et all, 2006.
 - "The eyes have it: A task by data type taxonomy of information
   visualizations"
   https://www.cs.umd.edu/users/ben/papers/Shneiderman1996eyes.pdf
 - "Information Visualization and Visual Data Mining", Daniel A. Keim

## Export results

 - Provide save-as-CSV from a runner?

## Sharing

First option was TogetherJS.  Now doubting, as it my be much better to
_not_ see exactly the same UI for cooperation.  What about

  - Allow for sharing editors
  - Allow sharing runners (= query+program)
  - Provide chat and whiteboard (Anne)

## Saving files

  - Look at [Dillinger markup editor](http://dillinger.io/)
  - Server saves:

    - lib/gitty.pl saves individual files versioned with meta-data.	[OK]
    - Save
      - Ask for commit message if saved again
    - Save as
      - Meta data
        - Public							[OK]
	- Name								[OK]
	- Title								[OK]
	- Author, E-mail
	- Description (codemirror/markdown?)				[OK]
	- Keywords (using `tagit', auto-completion)
      - Handle errors
      - Keep current meta-data						[OK]
      - Use current to fill the dialog
    - Load
      - Record meta-data						[OK]
    - Info
      - Display meta-data						[OK]
      - Access history
    - New
      - Remove current file notion					[OK]

## File info

  - Three tabs
    - Meta info shows meta info of current version
    - History shows commit log and allows switching version
      - Selecting a version using `zoom-in` updates `Meta data`
        and `Changes` and changes the title to "File <name>@hash"
    - Changes shows the diff

## Search

  - Implement typeahead search
    - For lines of the source code					[OK]
    - For predicates							[OK]
    - For public files							[OK]
  - Implement string search with result page
