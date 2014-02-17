#!/usr/bin/make

EMACS    = emacs
testlogs = test-fuzzy-regexp-el.log

all: test

test: $(testlogs)

test-%-el.log: test/test-%.el
	$(EMACS) --batch --quick --directory . \
	--eval '(progn (load-file "$<") (ert-run-tests-batch-and-exit))' \
	2>&1 | tee $@

clean:
	-rm -f $(testlogs)

.PHONY: all test clean
