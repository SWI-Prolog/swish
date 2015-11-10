JS=editor.js navbar.js answer.js pane.js query.js runner.js swish.js \
   config.js jswish.js
JSAPP=editor.js navbar.js answer.js pane.js query.js runnerapp.js swish.js \
   config.js jswishapp.js
CM=$(shell find codemirror -name '*.js')
JQ=jquery.jsdoc
CONF=conf.json
MAIN=swish.js
MAINAPP=swishapp.js

all:	 min require.js

#all:	 doc min require.js

doc::	$(CONF) $(JQ) $(JS)
	jsdoc -p -c $(CONF) $(JQ) $(JS)

min:	swish-min.js swish-minapp.js


swish-min.js:	build.js $(JS) $(CM)
	r.js -o build.js
	gzip -fk $@

swish-minapp.js:	buildapp.js $(JSAPP) $(CM)
	r.js -o buildapp.js
	gzip -fk $@

build.js: $(MAIN) build.js.in
	swipl mkjsbuild.pl $(MAIN) build.js.in

buildapp.js: $(MAINAPP) buildapp.js.in
	swipl mkjsbuildapp.pl $(MAINAPP) buildapp.js.in

require.js: ../bower_components/requirejs/require.js
	cp -p $< $@

clean:
	rm -f require.js swish-min.js swish-min.js.gz swish-minapp.js swish-minapp.js.gz build.js buildapp.js
