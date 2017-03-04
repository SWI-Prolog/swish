HOVER=text-hover.css text-hover.js
HINT=show-hint-eclipse.css templates-hint.css templates-hint.js
RENDER_SVGTREE=svg-tree-drawer.js
SPLITTERD=bower_components/jquery.splitter/js

all:	hover hint svgtree

patch:	$(SPLITTERD)/.patched

hover:	js/codemirror/addon/hover \
	$(addprefix js/codemirror/addon/hover/, $(HOVER))
hint:	js/codemirror/addon/hint \
	$(addprefix js/codemirror/addon/hint/, $(HINT))
svgtree: js/render/svg-tree-drawer.js

# CodeMirror extensions

js/codemirror/addon/hover:
	mkdir -p $@
js/codemirror/addon/hint:
	mkdir -p $@

js/codemirror/addon/%: 3rdparty/CodeMirror-XQuery/codemirror-extension/addon/%
	rsync -au $< $@

# Rendering primitives

js/render/svg-tree-drawer.js: 3rdparty/svg-tree-drawer/svg-tree-drawer.js
	rsync -au $< $@

# Patches

$(SPLITTERD)/.patched:
	patch $(SPLITTERD)/jquery.splitter-0.15.0.js patches/jsplitter.patch
	touch $@
