.PHONY: all clean run_test

MAKEFLAGS=sw

all:
	$(MAKE) -C apps/ all

clean:
	$(MAKE) -C apps/ clean

run_test:
	$(MAKE) -C apps/ run_test

