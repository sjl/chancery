.PHONY: vendor pubdocs test test-sbcl test-ccl test-ecl test-abcl

sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidocs = $(shell ls docs/*reference*.markdown)
heading_printer = $(shell which heading || echo 'true')

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Testing ---------------------------------------------------------------------
test: test-sbcl test-ccl test-ecl test-abcl

test-sbcl: vendor
	$(heading_printer) computer 'SBCL'
	sbcl --load test/run.lisp

test-ccl: vendor
	$(heading_printer) slant 'CCL'
	ccl --load test/run.lisp

test-ecl: vendor
	$(heading_printer) roman 'ECL'
	ecl --load test/run.lisp

test-abcl: vendor
	$(heading_printer) broadway 'ABCL'
	abcl --load test/run.lisp

# Documentation ---------------------------------------------------------------
$(apidocs): $(sourcefiles)
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

docs/build/index.html: $(docfiles) $(apidocs) docs/title
	cd docs && ~/.virtualenvs/d/bin/d

docs: docs/build/index.html

pubdocs: docs
	hg -R ~/src/docs.stevelosh.com pull -u
	rsync --delete -a ./docs/build/ ~/src/docs.stevelosh.com/chancery
	hg -R ~/src/docs.stevelosh.com commit -Am 'chancery: Update site.'
	hg -R ~/src/docs.stevelosh.com push
