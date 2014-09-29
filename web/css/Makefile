MAIN=swish.css
CSS=$(MAIN) editor.css pane.css query.css runner.css term.css

swish-min.css: $(CSS)
	cleancss -o $@ $(MAIN)
	swipl css-inline.pl $@
	gzip -fk $@

clean::
	rm -f swish-min.css swish-min.css.gz
