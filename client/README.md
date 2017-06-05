# Programming against SWISH

SWISH is a Prolog development environment built on top of the SWI-Prolog
[Pengines](http://pengines.swi-prolog.org)  library.  SWISH  allows  for
prototyping  and  sharing  query  patterns.    This   _readme_  provides
documentation and utilities to access  the   query  results  from remote
systems.

## Extracting result using the (Unix) shell

`swish-ask.sh` is a simple Unix  shell   (bash)  script  that allows for
asking queries against one or more saved  programs and return the result
as CSV. Run `swish-ask.sh` to get basic help.

`swish-ask.sh` requires [curl](http://curl.haxx.se/)

## Extracting result using Prolog

Prolog users can exploit the pengine API, in particular
[pengine_rpc/3](http://www.swi-prolog.org/pldoc/doc_for?object=pengines:pengine_rpc/3).
For example:

```{prolog}
?- [library(pengines)].
?- pengine_rpc('http://swish.swi-prolog.org',
	       sin_table(X,Y),
	       [ src_text(':- include(sin_table).'),
		 application(swish)
	       ]).
X = 0,
Y = 0.0 ;
X = 1,
Y = 0.01745240643728351 ;
X = 2
...
```

## Extracting results using JavaScript

The Pengines infrastructure is  designed  to   make  JavaScript  talk to
Prolog servers. The file   [sin-table.html](sin-table.html)  illustrates
this.

There an __NPM__ [package](https://www.npmjs.com/package/pengines)

## Extracting results using Java

Check out [JavaPengines](https://github.com/Anniepoo/JavaPengine)

## Extracting results using Ruby

Check out [RubyPengines](https://github.com/simularity/RubyPengine)

## Extracting results using Erlang

Check out [erl_pengine](https://github.com/Limmen/erl_pengine)

---
If you write or find another client, please make a pull request for this
page!
