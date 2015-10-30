MAIN=swish.css
CSS=$(MAIN) answer.css editor.css navbar.css pane.css query.css \
	    runner.css term.css
MAINAPP=swishapp.css
CSSAPP=$(MAINAPP) answer.css editor.css navbar.css paneapp.css query.css \
	    runner.css term.css

all: swish-min.css swish-minapp.css

swish-min.css: $(CSS)
	cleancss -o $@ $(MAIN)
	swipl css-inline.pl $@
	gzip -fk $@

swish-minapp.css: $(CSSAPP)
	cleancss -o $@ $(MAINAPP)
	swipl css-inline.pl $@
	gzip -fk $@


clean::
	rm -f swish-min.css swish-min.css.gz swish-minapp.css swish-minapp.css.gz
