# ZipAr

ZipAr is a fast alternative to `zip -0 -r --symlinks`.
It does **NOT** compress any files; instead,
it simply combines them into a single zip file,
using multiple threads for maximum speed.

## A quick benchmark

I ran a benchmark comparing zip(1) and ZipAr
on my setup (Ubuntu 24.04 LTS, Intel Core i7-8700, 32GiB RAM)
using ten 1GB files.

```
$ mkdir bench
$ cd bench
$ seq 10 | while read i; do fallocate -l 1G $i; done
$ time zip -0 -r ../zip . > /dev/null
zip -0 -r ../zip . > /dev/null  21.03s user 7.18s system 99% cpu 28.232 total
$ time zipar run ../zip2 .
../_build/default/bin/main.exe run ../zip2 .  14.67s user 18.52s system 315% cpu 10.529 total
```

As you can see, ZipAr ran about **2.7 times faster** than zip(1).
Of course, this is a quick benchmark, and I plan to run
more thorough benchmarks soon.

## How to setup your environment to develop zipar

Install opam >= 2.2.0 (for `--with-dev-setup`). Then:

```
cd zipar
make setup-dev
make
make test
```
