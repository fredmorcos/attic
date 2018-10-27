GCC   ?= gcc
PSCC  ?= pathcc
CLANG ?= clang

COMMON_CFLAGS = -pedantic-errors -O3 -Wall -Werror -std=c99 -g0
GCC_CFLAGS    =	-pedantic $(COMMON_CFLAGS)
PSCC_CFLAGS   =	$(COMMON_CFLAGS)
CLANG_CFLAGS  = $(GCC_CFLAGS)

all: testbench-gcc testbench-pscc testbench-clang

%.gcco: %.c
	$(GCC) $< -c -o $@ $(GCC_CFLAGS) -D'__CFLAGS__="$(GCC_CFLAGS)"'

%.pscco: %.c
	$(PSCC) $< -c -o $@ $(PSCC_CFLAGS) -D'__CFLAGS__="$(PSCC_CFLAGS)"'

%.clango: %.c
	$(CLANG) $< -c -o $@ $(CLANG_CFLAGS) -D'__CFLAGS__="$(CLANG_CFLAGS)"'

testbench-gcc: matrix.gcco bench.gcco testbench.gcco
	$(GCC) -o $@ $^ $(GCC_CFLAGS)

testbench-pscc: matrix.pscco bench.pscco testbench.pscco
	$(PSCC) -o $@ $^ $(PSCC_CFLAGS)

testbench-clang: matrix.clango bench.clango testbench.clango
	$(CLANG) -o $@ $^ $(CLANG_CFLAGS) -D'__CFLAGS__="$(CLANG_CFLAGS)"'

clean:
	rm -rf *.gcco *.pscco *.clango testbench-gcc testbench-pscc testbench-clang *~