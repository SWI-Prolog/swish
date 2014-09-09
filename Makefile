all: css js

css::
	@$(MAKE) -C web/css

js::
	@$(MAKE) -C web/js

clean::
	@$(MAKE) -C web/css clean
	@$(MAKE) -C web/js clean

