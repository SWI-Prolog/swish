MAIN=swish.css
CSS=$(MAIN) answer.css editor.css navbar.css pane.css query.css \
	    runner.css term.css

all:	swish-min.css swish-min.css.gz

swish-min.css: $(CSS)
	cleancss -o $@ $(MAIN)
	swipl css-inline.pl $@

swish-min.css.gz: swish-min.css
	gzip < $< > $@

clean::
	rm -f swish-min.css swish-min.css.gz
