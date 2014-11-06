# SWISH: A web based SWI-Prolog environment

## Installation

### Get JavaScript requirements

#### Using bower

Install [bower](http://bower.io) for your  platform.   On  Ubuntu,  this
implies getting `node` and `npm` by installing two packages and next use
`npm` to install `bower`:

    sudo apt-get install npm nodejs-legacy
    sudo npm install -g bower

Once you have `bower`, run the following from the toplevel of `swish` to
get the dependencies:

    bower install

#### Download as zip

As installing node and bower is not a pleasure on all operating systems,
you can also download  the  dependencies  as   a  single  zip  file from
http://www.swi-prolog.org/download/swish/swish-bower-components.zip.
Unpack the zip file, maintaining the directory structure, from the swish
root directory to create the directory web/bower_components.


### Get the latest SWI-Prolog

Install the latest  [SWI-Prolog](http://www.swi-prolog.org) _development
version_. As SWISH is very  much  in   flux  and  depends  on the recent
SWI-Prolog pengines and sandboxing libraries, it   is  quite common that
you            need            the             [nightly            build
(Windows)](http://www.swi-prolog.org/download/daily/bin/) or build   the
system    from    the     current      git     development    repository
[swipl-devel.git](https://github.com/SWI-Prolog/swipl-devel).

Nov 6, 2014: release 7.1.26 fully supports the current SWISH.


## Running SWISH

With a sufficiently recent Prolog installed, start the system by opening
`run.pl` either by running `swipl  run.pl`   (Unix)  or opening `run.pl`
from the Windows explorer.

Now direct your browser to http://localhost:3050/

If you want  to  know  what  the   latest  version  looks  like,  go  to
http://swish.swi-prolog.org/


### Running SWISH without sandbox limitations

By default, SWISH lets you only run _safe_  commands. If you want to use
SWISH for unrestricted development, load the authentication module:

    ?- [lib/authenticate].

Next, for first usage, you need  to   create  a user. The authentication
module defines swish_add_user/3, which updates or  creates a file called
`passwd`:

    ?- swish_add_user(guru, 'top secret', []).

If you now try to run a command in  SWISH, it will prompt for a user and
password. After authentication you can run any Prolog predicate.

**NOTE** Authentication uses plain HTTP   basic authentication. Only use
this on trusted networks and do not  use   a  password  that you use for
other sensitive services. If you want to  setup a public server this way
you are strongly adviced to use HTTPS.


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

No building is needed  to  run  the   system  from  sources.  For public
installations you probably want to create   the  minified JavaScript and
CSS files to reduce network traffic and startup time. You need some more
tools for that:

    % [sudo] npm install -g jsdoc
    % [sudo] npm install -g requirejs
    % [sudo] npm install -g clean-css

You also need GNU make installed as   `make`  and SWI-Prolog as `swipl`.
With all that in  place,  the   following  command  creates the minified
versions:

    % make

The default main page (`/`)  is   generated  from `lib/page.pl`. It uses
minified    JavaScript    and    CSS      from     `web/js/swish-min.js`
`web/css/swish-min.css` when available. If the   minified  files are not
present,  the  server  automatically  includes   the  full  source.  The
generated files may be removed using

    make clean

Alternatively, use of the minified  files   can  be  disable from Prolog
using this command and reloading the page:

    ?- debug(nominified).

## Documentation

The JavaScript is documented   using  [JsDoc](http://usejsdoc.org/). The
generated documentation is available in `web/js/doc/index.html`.
