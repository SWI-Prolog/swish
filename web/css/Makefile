MAIN=swish.css
CSS=$(MAIN) editor.css pane.css query.css runner.css term.css

swish-min.css: $(CSS)
	cleancss -o $@ $(MAIN)

clean::
	rm -f swish-min.css
