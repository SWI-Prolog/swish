MAIN=swish.css
CSS=$(MAIN) \
	answer.css    editor.css  modal.css	query.css   term.css  \
	chat.css      form.css	  navbar.css	runner.css  typeaheadjs.css \
	chatroom.css  gitty.css   notebook.css	types.css   flash.css \
	diffview.css  login.css   pane.css	tabbed.css

all:	swish-min.css swish-min.css.gz

swish-min.css: $(CSS)
	cleancss -o $@.new $(MAIN)
	swipl css-inline.pl $@.new
	mv $@.new $@

swish-min.css.gz: swish-min.css
	gzip < $< > $@

clean::
	rm -f *~ swish-min.css swish-min.css.gz
