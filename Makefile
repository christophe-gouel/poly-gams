EMACS ?= emacs

# `package-initialize' supplies the dependencies (polymode, gams-mode and,
# optionally, yaml-mode) from the user's package directory.  `-L .' comes after
# it so that this checkout shadows any installed copy of poly-gams.
BATCH = $(EMACS) -Q --batch --eval '(package-initialize)' -L . -L test

.PHONY: test compile clean

test:
	$(BATCH) -l test/poly-gams-tests.el -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile poly-gams.el

clean:
	rm -f *.elc test/*.elc
