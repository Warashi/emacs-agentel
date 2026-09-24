EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L . -L test

SOURCES = $(wildcard agentel*.el)
TESTS = $(wildcard test/*-test.el)

.PHONY: test lint compile checkdoc clean

test:
	$(BATCH) -l ert $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

lint: compile checkdoc

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SOURCES)
	rm -f $(SOURCES:.el=.elc)

# checkdoc-file only prints warnings, so fail when any is printed.
checkdoc:
	@out=$$($(BATCH) --eval '(progn (dolist (f command-line-args-left) (checkdoc-file f)) (setq command-line-args-left nil))' \
	  $(SOURCES) 2>&1); \
	echo "$$out"; \
	! echo "$$out" | grep -q '^Warning'

clean:
	rm -f *.elc test/*.elc
