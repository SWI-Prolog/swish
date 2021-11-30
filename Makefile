# Copyright: VU University of Amsterdam, CWI Amsterdam
# License:   Simplified BSD license

YARN_ARCHIVE=swish-node-modules.zip
YARN_URL=https://www.swi-prolog.org/download/swish/${YARN_ARCHIVE}
SWIPL=swipl

# Packs to download and configure.  Run `git submodule` to see the
# available packs.
PACKS=profile rserve_client smtp pcache sCASP

all:
	@echo "Targets"
	@echo
	@echo "    yarn-zip  -- Download zip with dependencies"
	@echo "    yarn	     -- Install dependencies using yarn"
	@echo "    src       -- Prepare JavaScript dependencies for execution"
	@echo "    min       -- Create minimized CSS and JavaScript"
	@echo "    clean     -- Clean minimized CSS and JavaScript"
	@echo "    packs     -- Download and configure packs"
	@echo

yarn::
	yarn
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

# Install dependencies from downloaded zip holding JavaScript components

yarn-zip: .yarn-senitel
.yarn-senitel: $(YARN_ARCHIVE)
	unzip -q -u $(YARN_ARCHIVE)
	touch $@

$(YARN_ARCHIVE)::
	@if [ -e $(YARN_ARCHIVE) ]; then \
	  curl -o $(YARN_ARCHIVE) -z $(YARN_ARCHIVE) $(YARN_URL) ; \
	else \
	  curl -o $(YARN_ARCHIVE) $(YARN_URL) ;\
	fi

# Create the above

upload::
	rm -f $(YARN_ARCHIVE)
	zip -r $(YARN_ARCHIVE) web/node_modules
	rsync $(YARN_ARCHIVE) ops:/home/swipl/web/download/swish/$(YARN_ARCHIVE)


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
