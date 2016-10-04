# Copyright: VU University of Amsterdam, CWI Amsterdam
# License:   Simplified BSD license

BOWER_ARCHIVE=swish-bower-components.zip
BOWER_URL=http://www.swi-prolog.org/download/swish/${BOWER_ARCHIVE}

all: css js

css::
	@$(MAKE) -C web/css

js::
	@$(MAKE) -C web/js

src::
	@$(MAKE) -C web/js src

clean::
	@$(MAKE) -C web/css clean
	@$(MAKE) -C web/js clean

bower-components::
	curl $(BOWER_URL) > $(BOWER_ARCHIVE)
	unzip -u $(BOWER_ARCHIVE)
	rm $(BOWER_ARCHIVE)

swish-bower-components.zip::
	rm -f $@
	zip -r $@ web/bower_components

upload:	swish-bower-components.zip
	rsync swish-bower-components.zip ops:/home/swipl/web/download/swish/swish-bower-components.zip

