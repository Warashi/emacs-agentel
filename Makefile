EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L . -L test

SOURCES = $(wildcard agentel*.el)
TESTS = $(wildcard test/*-test.el)

PYTHON ?= python3
SPEC = test/spec

.PHONY: test test-spec spec lint compile checkdoc clean

test: test-spec
	$(BATCH) -l ert $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

test-spec:
	PYTHONDONTWRITEBYTECODE=1 $(PYTHON) -m unittest discover -s $(SPEC)

# Regenerate the test cases taken from the specs, to be reviewed.
spec:
	$(PYTHON) $(SPEC)/convert.py $(SPEC)/commonmark-0.31.2.json $(SPEC)/gfm-27d942c.txt \
	  > $(SPEC)/expected.eld

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
