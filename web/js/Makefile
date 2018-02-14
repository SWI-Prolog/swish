JS=answer.js diffview.js jswish.js preferences.js tabbed.js download.js \
   links.js prolog.js term.js chat.js editor.js login.js query.js utils.js \
   chatroom.js modal.js runner.js config.js form.js navbar.js search.js \
   diff.js gitty.js notebook.js storage.js difflib.js history.js pane.js \
   swish.js sourcelist.js
CM=$(shell find codemirror -name '*.js')
JQ=jquery.jsdoc
CONF=conf.json
MAIN=swish.js
RJS=r.js

all:	src min require.js

src::
	$(MAKE) -C ../bower_components/sparkline jqs

doc::	$(CONF) $(JQ) $(JS)
	-jsdoc -p -c $(CONF) $(JQ) $(JS)

min:	swish-min.js swish-min.js.gz

swish-min.js:	build.js $(JS) $(CM)
	rm -f swish-min.js.new swish-min.js.new.map
	$(RJS) -o build.js
	sed -i 's@//# sourceMappingURL=swish-min.js.new.map@//# sourceMappingURL=swish-min.js.map@' swish-min.js.new
	mv swish-min.js.new swish-min.js
	if [ -f swish-min.js.new.map ]; then \
	  mv swish-min.js.new.map swish-min.js.map; \
	fi

swish-min.js.gz: swish-min.js
	gzip < $< > $@

build.js: $(MAIN) build.js.in
	swipl mkjsbuild.pl $(MAIN) build.js.in

require.js: ../bower_components/requirejs/require.js
	cp -p $< $@

clean:
	rm -f *~ require.js swish-min.js swish-min.js.new swish-min.js.gz
