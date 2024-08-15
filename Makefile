.PHONY: build
build:
	dune build

.PHONY: test
test:
	$(MAKE) build
	if [ ! -d test/testdata ]; then $(MAKE) generate-testdata; fi
	dune runtest

.PHONY: setup
setup:
	opam install . --deps-only --with-test

.PHONY: setup-dev
setup-dev:
	opam install . --deps-only --with-dev-setup --with-test

.PHONY: generate-testdata
generate-testdata:
	rm -rf test/testdata
# testdir1
	mkdir -p test/testdata/testdir1/a
	echo "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa" > test/testdata/testdir1/a/b
	echo "ccccccccccccccccccccccccccccc" > test/testdata/testdir1/c
	cd test/testdata/testdir1; rm -f ../testdir1-inside.zip; zip -0 -r ../testdir1-inside.zip .
