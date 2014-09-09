# SWISH JavaScript components

## Technology stack

The SWISH JavaScript  components  are   jQuery  plugins.  Our technology
toolchain consists of the following components:

  - *jQuery* is the core of our technology stack
  - *Bootstrap* is used for some layout components
  - *RequireJS* is used to deal with code dependencies
  - *JsDoc* is used to comment the code
  - *clean-css* is used to minify the CSS and inline the
    referenced images.

## Components

  - Page handling

    - `pane.js` splits the page in tiles.  This library needs to be
      extended to deal with adding, moving and removing tiles.
    - `navbar.js` builds the navigation bar, based on Bootstrap.
    - `modal.js` provide modal dialogs, build on top of Bootstrap.

  - Prolog components

    - `editor.js` embeds CodeMirror.  Our Prolog extensions for CodeMirror
      are in the local `codemirror` directory.
    - `query.js` allows for editing a query and managing history and
      examples.  It uses `editor.js` for the query editing.
    - `runner.js` provides the infrastructure for execting a query using
      a pengine, including controlling the pengine and using `answer.js`
      for rendering answers.
    - `answer.js` is (now) a simple plugin that renders a single Prolog
      answer.
    - `term.js` is a simple term fold/unfold component.

  - Putting it all together

    - `config.js` extracts configuration information from the server,
      notably the HTTP locations for specific handlers.  This is used
      rather than hardcoding paths to allow for _rebasing_ the SWISH
      component.
    - `swish.js` creates the overall page. Needs further modularization.

  - Tool support

    - `build.js` contains the `r.js` declarations for minifying the
      JavaScript.
    - `conf.json` contains the JsDoc configuration/
    - `empty.js` contains a jQuery plugin *skeleton*.  There are many
      ways to write jQuery plugins.  This one can be understood by
      JsDoc and is fiarly close to writing a normal JavaScript class.

## Documentation generation

Documentation is generated using JsDoc.  The   commandline  tools can be
installed using npm:

    % npm install -g jsdoc

The documentation can be  build  using   `make  doc`,  which creates the
directory `doc`.

## Minifying

Minification is based on `r.js`, which may be installed using `npm`:

    % npm install -g requirejs

The minified file `swish-min.js` can be build using `make min`
