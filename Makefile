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
# testdir2
	mkdir -p test/testdata/testdir2
	echo "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa" > test/testdata/testdir2/a
	fallocate -l 5G test/testdata/testdir2/file3g
	echo "zzzzzzzzzzzzzzzzzzzzzzzzzzzzz" > test/testdata/testdir2/z
# testdir3
	mkdir -p test/testdata/testdir3
	seq 65535 | while read i; do touch test/testdata/testdir3/$${i} & done; wait
# testdir4
	mkdir -p test/testdata/testdir4
	seq 65536 | while read i; do touch test/testdata/testdir4/$${i} & done; wait
# testdir5
	mkdir -p test/testdata/testdir5
	echo "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa" > test/testdata/testdir5/a
	cd test/testdata/testdir5; ln -s a b; ln -s /bin/sh c
