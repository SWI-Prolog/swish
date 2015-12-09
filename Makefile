all: css js

css::
	@$(MAKE) -C web/css

js::
	@$(MAKE) -C web/js

clean::
	@$(MAKE) -C web/css clean
	@$(MAKE) -C web/js clean

swish-bower-components.zip::
	rm -f $@
	zip -r $@ web/bower_components

upload:	swish-bower-components.zip
	rsync swish-bower-components.zip ec:/home/swipl/web/download/swish/swish-bower-components.zip

