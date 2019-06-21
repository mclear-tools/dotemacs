emacs ?= emacs

LOAD = -l gited-ci.el -l gited.el

all: test

test:
	$(emacs) -batch $(LOAD) -l gited-tests.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -batch --eval "(progn (add-to-list 'load-path default-directory) (byte-compile-file \"gited.el\"))"

clean:
	rm -f *.elc

.PHONY: all compile clean test
