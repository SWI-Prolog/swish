# Copyright: VU University of Amsterdam, CWI Amsterdam
# License:   Simplified BSD license

BOWER_ARCHIVE=swish-bower-components.zip
BOWER_URL=http://www.swi-prolog.org/download/swish/${BOWER_ARCHIVE}
SWIPL=swipl

# Packs to download and configure.  Run `git submodule` to see the
# available packs.
PACKS=profile rserve_client smtp pcache

all:
	@echo "Targets"
	@echo
	@echo "    bower-zip -- Download zip with bower dependencies"
	@echo "    bower     -- Install dependencies using bower"
	@echo "    src       -- Prepare bower dependencies for execution"
	@echo "    min       -- Create minimized CSS and JavaScript"
	@echo "    clean     -- Clean minimized CSS and JavaScript"
	@echo "    packs     -- Download and configure packs"
	@echo

bower::
	bower install
	@$(MAKE) src

src::
	@$(MAKE) -C web patch
	@$(MAKE) -C web/js src

# Build and clean minimized versions

min:: css js

css::
	@$(MAKE) -C web/css

js::
	@$(MAKE) -C web/js

clean::
	@$(MAKE) -C web/css clean
	@$(MAKE) -C web/js clean

# Install dependencies from downloaded zip holding bower components

bower-zip: .bower-senitel
.bower-senitel: $(BOWER_ARCHIVE)
	unzip -q -u $(BOWER_ARCHIVE)
	touch $@

$(BOWER_ARCHIVE)::
	@if [ -e $(BOWER_ARCHIVE) ]; then \
	  curl -o $(BOWER_ARCHIVE) -z $(BOWER_ARCHIVE) $(BOWER_URL) ; \
	else \
	  curl -o $(BOWER_ARCHIVE) $(BOWER_URL) ;\
	fi

# Create the above

upload::
	rm -f $(BOWER_ARCHIVE)
	zip -r $(BOWER_ARCHIVE) web/bower_components
	rsync $(BOWER_ARCHIVE) ops:/home/swipl/web/download/swish/$(BOWER_ARCHIVE)


################
# PACKS

PACKFILES=$(addprefix pack/, $(addsuffix /pack.pl, $(PACKS)))
ATTACH_PACKDIR=-g 'attach_packs(pack,[duplicate(replace),search(first)])'

packs: $(PACKFILES)

$(PACKFILES)::
	@echo "Checking $(shell dirname $@) ..."
	@if [ ! "`git submodule status $(shell dirname $@) | head -c 1`" = " " ]; then \
	  echo "  Updating $(shell dirname $@) ..." ; \
	  git submodule update --init $(shell dirname $@) ; \
	  $(SWIPL) $(ATTACH_PACKDIR) -g 'pack_rebuild($(shell basename $$(dirname $@)))' -t halt ;\
	fi
