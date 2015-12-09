MAIN=swish.css
CSS=$(MAIN) \
	answer.css diffview.css editor.css gitty.css modal.css \
	navbar.css notebook.css pane.css runner.css \
	tabbed.css term.css typeaheadjs.css types.css

all:	swish-min.css swish-min.css.gz

swish-min.css: $(CSS)
	cleancss --skip-restructuring -o $@ $(MAIN)
	swipl css-inline.pl $@

swish-min.css.gz: swish-min.css
	gzip < $< > $@

clean::
	rm -f swish-min.css swish-min.css.gz
