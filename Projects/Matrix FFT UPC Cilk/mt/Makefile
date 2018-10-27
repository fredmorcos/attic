# UPC C Flags = -O
# ICC C Flags = -O3

# ICCFLAGS=-axSSE4.2
# CFLAGS=-O3 -Wall -Werror $(ICCFLAGS)
# CILKLDFLAGS=-lcilkrts

# MTFLAGS=-DMATN=$(MATSIZE) -DPSQRT=$(PSQRT) -DBSIZE=$(BSIZE)

# UPCNET=-network=$(NETWORK)
# UPCFLAGS=-O $(UPCNET) -T=$(THREADS) # -Wc,"$(CFLAGS)"
# UPCLDFLAGS=-link-with=$(CC)

# LDFLAGS=-lrt
# UPCRFLAGS=$(QUIET) $(UPCNET) -shared-heap=$(HEAPS) -c $(PN) -n $(THREADS)

UPCC	= upcc
UPCR	= upcrun
CC	= icc
NETWORK = ibv

ICC_BUILD_CMD = $(CC)   -Wall -Werror -axSSE4.2 -fPIC
UPC_BUILD_CMD = $(UPCC) -network=$(NETWORK) -T=$(UPC_THREADS)
UPC_RUN_CMD   = $(UPCR) -network=$(NETWORK) -shared-heap=$(HEAP_SIZE) \
		        -c $(CPUS_PER_NODE) -n $(UPC_THREADS)
MT_SETTINGS = -DPSQRT=$(PSQRT) -DBUFFER_SIZE=$(BUFFER_SIZE) -DLS=$(LS)

TARGETS = mt-single-buffer-cilkp # mt2c+ mt3c+

all: $(TARGETS)

mt-cilkp.o: mt-cilkp.c
	$(ICC_BUILD_CMD) $(MT_SETTINGS) -c -o $@ $<

mt-single-buffer-cilkp.o: mt-single-buffer-cilkp.upc
	$(UPC_BUILD_CMD) $(MT_SETTINGS) -c -o $@ $<

mt-single-buffer-cilkp: mt-single-buffer-cilkp.o mt-cilkp.o
	$(UPC_BUILD_CMD) -link-with=$(CC) -lcilkrts -o $@ $^

run-mt-single-buffer-cilkp: mt-single-buffer-cilkp
	(export CILK_NWORKERS=$(CILK_THREADS); \
	 export UPC_CILK_NWORKERS=$(CILK_THREADS); \
	 $(UPC_RUN_CMD) $<)

# mt2c+.o: mt-2cbuf-cilk+.upc
# 	upcc -network=ibv -T=$(THREADS) \
# 	     -DMATN=$(MATSIZE) -DPSQRT=$(PSQRT) -DBSIZE=$(BSIZE) \
# 	     -c -o $@ $<

# mt2c+: mt2c+.o cilk+-transpose.o
# 	upcc -network=ibv -T=$(THREADS) \
# 	     -link-with=$(CC) \
# 	     -lcilkrts -o $@ $^

# run-mt2c+: mt2c+
# 	(export CILK_NWORKERS=$(CILKTHREADS); \
# 	 export UPC_CILK_NWORKERS=$(CILKTHREADS); \
# 	 $(UPCR) $(UPCRFLAGS) $<)

# mt3c+.o: mt-3cbuf-cilk+.upc
# 	upcc -network=ibv -T=$(THREADS) \
# 	     -DMATN=$(MATSIZE) -DPSQRT=$(PSQRT) -DBSIZE=$(BSIZE) \
# 	     -c -o $@ $<

# mt3c+: mt3c+.o cilk+-transpose.o
# 	upcc -network=ibv -T=$(THREADS) \
# 	     -link-with=$(CC) \
# 	     -lcilkrts -o $@ $^

# run-mt3c+: mt3c+
# 	(export CILK_NWORKERS=$(CILKTHREADS);     \
# 	 export UPC_CILK_NWORKERS=$(CILKTHREADS); \
# 	 $(UPCR) $(UPCRFLAGS) $<)

.PHONY: clean
clean:
	rm -f *.o core $(TARGETS)
