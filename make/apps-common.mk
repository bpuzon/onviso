# This line is commented out, so you can override the defaults in your
# own makefile.
# include ../../make/apps-defs.mk

.PHONY: clean run_test
all: $(BEAMS) app doc
run_test: test

empty:=
space:=$(empty) $(empty)
comma:=,

# Build application resource file.  But don't complain if the .app
# file exists while its prerequisites do not.
app: $(APPFILE)
$(APPFILE): src/$(APPLICATION).app.src vsn.mk
	@echo "APP $@"
# create the ebin directory if it doesn't exist
	@mkdir -p $(dir $@)
# insert the list of modules into the .app file, quoting them all in
# case some of them start with capital letters
	@test ! -f $< || \
	sed -e "s|[@%]MODULES[@%]|$(subst $(space),$(comma),$(foreach module,$(MODULES),\'$(module)\'))|g" \
		-e "s|[@%]VSN[@%]|$(VSN)|" $< > $@
src/$(APPLICATION).app.src vsn.mk:
	@true

# Build an erlang file
ebin/%.beam: src/%.erl $(APPFILE) 
	@echo ERLC $<
	@erlc $(ERLC_FLAGS) -I include -o ebin $<

RUN_TEST_PATH=  -pa ../eqc/ebin \
		-pa "$(PWD)/ebin" \
		-pa "$(PWD)/test" 

run_test: test 
	@echo ERLTEST $(APPLICATION)_suite
	$(ERLTEST) $(ERLTEST_FLAGS) -outdir $(COVER_REPORT) \
		$(RUN_TEST_PATH) \
		-cover src/ test/

run_test_shell:
	erl $(RUN_TEST_PATH)

run_gui: all 
	@echo ERLGUI $(APPLICATION)
	erl -pa "$(PWD)/ebin" -eval "wxi:start()"

test: all $(TEST_BEAMS)

test/%.beam: test/%.erl 
	@echo ERLC $<
	@erlc $(TEST_ERLC_FLAGS) -I include -o test $<

# Build docs
doc: $(DOCS)
$(DOCS): $(SOURCES) $(INCLUDES) doc/overview.edoc $(APPFILE)
	@echo "EDOC $(APPLICATION)"
	@erl -noinput -eval \
		'edoc:application($(APPLICATION), "./", [{doc, "doc/"}, {preprocess, true}, {todo, true}]).' \
		-s erlang halt

# Clean
clean:
	@echo "Cleaning"
	@rm -f doc/*.html doc/edoc-info doc/erlang.png doc/stylesheet.css
	@test ! -f src/$(APPLICATION).app.src || rm -f ebin/$(APPLICATION).app
	@rm -f ebin/*.beam
	@rm -f test/*.beam
	@rm -rf cover_report
