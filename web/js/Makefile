JS=editor.js navbar.js answer.js pane.js query.js runner.js swish.js \
   config.js jswish.js search.js history.js preferences.js tabbed.js storage.js \
   notebook.js prolog.js
CM=$(shell find codemirror -name '*.js')
JQ=jquery.jsdoc
CONF=conf.json
MAIN=swish.js

all:	src min require.js

src::
	$(MAKE) -C ../bower_components/sparkline jqs

doc::	$(CONF) $(JQ) $(JS)
	-jsdoc -p -c $(CONF) $(JQ) $(JS)

min:	swish-min.js swish-min.js.gz

swish-min.js:	build.js $(JS) $(CM)
	r.js -o build.js

swish-min.js.gz: swish-min.js
	gzip < $< > $@

build.js: $(MAIN) build.js.in
	swipl mkjsbuild.pl $(MAIN) build.js.in

require.js: ../bower_components/requirejs/require.js
	cp -p $< $@

clean:
	rm -f require.js swish-min.js swish-min.js.gz
