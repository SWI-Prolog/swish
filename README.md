# SWISH: A web based SWI-Prolog environment

## Installation & Running

Install  [bower](http://bower.io)  for  your  platform    and  get  the
dependencies using the following command:

    bower install

Install [SWI-Prolog](http://www.swi-prolog.org) version 7.1.21  or later
for your platform and open `run.pl`,   either  by running `swipl run.pl`
(Unix) or opening `run.pl` from the Windows explorer.

Now direct your browser to http://localhost:3050/

## Design

Most of the application is realised  using client-side JavaScript, which
can be found  in  the  directory   `web/js`.  The  JavaScript  files use
[RequireJS](http://requirejs.org/)   for   dependency     tracking   and
[jQuery](http://jquery.com/) for structuring the   JavaScript  as jQuery
plugins. The accompanying CSS is in   `web/css`.  More details about the
organization of the JavaScript is in `web/js/README.md`

There are two overal pages. `web/swish.html`  provides a static page and
`lib/page.pl` provides a Prolog frontend to  generate the overal page or
parts thereof dynamically. The latter   facilitates smoothless embedding
in SWI-Prolog web applications.

## Development and debugging

The  default  main  page   (`/swish/index.html`)    is   generated  from
`lib/page.pl`.   It   uses   minified   JavaScript     and    CSS   from
`web/js/swish-min.js`  `web/css/swish-min.css`  when   available.  These
files are build by running `make`  in   this  directory. If the minified
files are not present,  the  server   automatically  includes  the  full
source.  The generated files may be removed using

    make clean

Alternatively, use of the minified  files   can  be  disable from Prolog
using this command and reloading the page:

    ?- debug(nominified).

## Documentation

The JavaScript is documented   using  [JsDoc](http://usejsdoc.org/). The
generated documentation is available in `web/js/doc/index.html`.

# Bugs

 - Examples dropup hides behind pane.
