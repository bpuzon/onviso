LIBDIRS:=$(shell pwd)

MAKEFLAGS=sw

APP_SRC:=$(shell find -L . -maxdepth 3 \( -name '*.app.src' -o -name '*.app' \) -print)
APPS:=$(sort $(shell echo $(APP_SRC) | xargs -n 1 echo | sed -e 's|^\./||' -e 's|/.*$$||'))

.PHONY: all clean app test run_test dialyzer

all: $(addsuffix .all,$(APPS))

# The function submake takes one argument: the directory in which to
# run make.  If this directory does not contain a Makefile, it uses
# make/apps-defs.mk and make/apps-common.mk instead.
submake = $(MAKE) -C $1 \
	  $(shell test -f $1/Makefile || \
	    echo -f \""$(LIBDIRS)/../make/apps-defs.mk"\" \
		 -f \""$(LIBDIRS)/../make/apps-common.mk"\")

%.all: %
	@$(call submake,$<) all

clean: $(addsuffix .clean,$(APPS))
	rm -f .depend

%.clean: %
	@$(call submake,$<) clean

test: all $(addsuffix .test,$(APPS))

%.test: %
	@$(call submake,$<) test

run_test: test $(addsuffix .run_test,$(APPS))

%.run_test: %
	@$(call submake,$<) run_test

app: $(addsuffix .app,$(APPS))

%.app: %
	@$(call submake,$<) app



DIALYZER_FLAGS?=-Wunderspecs
dialyzer:
	dialyzer $(DIALYZER_FLAGS) --src -r `find -L . -name src -print`

dialyzer_ignoreerrors:
	-dialyzer $(DIALYZER_FLAGS) --src -r `find -L . -name src -print`
