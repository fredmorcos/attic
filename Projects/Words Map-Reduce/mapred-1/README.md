# Introduction

A multi-threaded map-reduce implementation in C to count the
occurrences of words in a file.

# Building and running:

## Dependencies

This program depends on `hashmap`. The Makefile expects it to be under
the directory `hashmap`. See the `.gitmodules` file.

To build `hashmap` as the Makefile would expect it:

```sh
cd hashmap
mkdir build
cd build
cmake ..
make
```

See the section `dependencies` in the Makefile.

`hashmap`: https://github.com/DavidLeeds/hashmap

## Building

Running `make` will build all variants of this program. A release
build in `mapred`, a debug build in `mapred_debug`, a build with
pointer checking enabled in `mapred_ptrcheck` and sanitizer builds in
different `mapred_san*` executables.

Note that `make` will build using the default system compiler set in
`$(CC)`, to use clang run `CC=clang make` and to use gcc run `CC=gcc
make`.

`make clean` will take care of removing the binaries and their
respective object files.

## Running

Add the `hashmap/build` directory to `LD_LIBRARY_PATH`:

`LD_LIBRARY_PATH=$LD_LIBRARY_PATH:hashmap/build ./mapred FILE N_THREADS`

## Running with Valgrind

`LD_LIBRARY_PATH=$LD_LIBRARY_PATH:hashmap/build valgrind ./mapred_debug FILE N_THREADS`

## Fuzzing with AFL:

To build for AFL fuzzing: `CC=afl-clang make` or `CC=afl-gcc make`.

To fuzz in parallel, run the following in separate terminals:

```sh
$ afl-fuzz -t 500 -i tests.in -o afl -M f1 ./mapred @@ 5
$ afl-fuzz -t 500 -i tests.in -o afl -S f2 ./mapred @@ 5
$ afl-fuzz -t 500 -i tests.in -o afl -S f3 ./mapred @@ 5
$ afl-fuzz -t 500 -i tests.in -o afl -S f4 ./mapred @@ 5
```

# Notes:

- A trie might be more efficient than a hashmap for the book-keeping
  of word lists.

- I explicitly avoided VLAs due to being dangerous and I think also
  slower in some cases.

- I am unsure on how punctuation, spacing, upper and lowercase
  would be handled for languages other than English.

- This will not work for composed words (with a dash in
  between). `foo-bar` will be handled as two words `foo` and `bar`
  instead of a single word `foobar`.

- This assumes that case does not matter: `Foo` and `foo` are
  considered the same word.

# Todo:

- [x] Profiling, find out why strcmp() is called too often.

- [ ] Implement benchmarks and plot them.

- [x] Find a good dataset for benchmarking:

  - https://github.com/dwyl/english-words/blob/master/words_alpha.txt

```sh
curl -# -O https://github.com/dwyl/english-words/blob/master/words_alpha.txt
for i in `seq 1 500`; do
    cat words_alpha.txt | tr '\n' ' ' >> words_big.txt
done
```

- [x] Do not wait for all threads to join before merging results.

- [x] Implement hierarchical map-reduce, rather than having a single
      reduce step, implement a fork-join style of merging results
      pairwise. This was not very benficial, in fact it ended up being
      slower, since the single-threaded reduce step was not consuming
      any time when processing the same dataset, but I left it in in
      the name of "scalability".

- [ ] Merge the map and reduce steps into a fork-join work-stealing
      structure would be the most beneficial, where the input buffer
      is recursively split and as the counting rountines finish, their
      results are merged back together hierarchically. Something
      similar to `Cilk`, like `Lace` or `OpenMP` might be useful for
      this.

`Lace`: https://github.com/trolando/lace

- [x] Restructure the code and improve the readability and
      documentation.

- [x] Improve the structure of the algorithm, break it down into
      smaller functions.

- [ ] Make sure to maximize CPU utilization, currently it is at about
      ~65% or ~70% and I am not sure why. Also try to find other gaps
      in CPU utilization.

- [ ] Reduce the number of heap allocations.
