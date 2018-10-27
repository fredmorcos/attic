UPCC = upcc
UPCRUN = upcrun
UPCNET = ibv

CFLAGS = -O 
CC = icc

CILKCC = /home/gup/k333770/cilk/bin/cilk++
CILK++ = /home/gup/k333770/cilk/lib

CILKDIR = /home/ica/k333770/mycilk/

CILKC = $(CILKDIR)/bin/cilkc
CILKDEFS = -DHAVE_CONFIG_H
CILKINCL = -I. -I$(CILKDIR)/include/cilk

libcilkmt.so:
	$(CILKCC) -O3 -o cilk-transpose.o -c cilk-transpose.cilk -fPIC -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -DLEAFSIZE=$(LS)
	$(CILKCC) -shared -o libcilkmt.so cilk-transpose.o

mtc++:	mt-cbuf-cilk.upc libcilkmt.so
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -T=$(THREADS) -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -o mtc++.o -c mt-cbuf-cilk.upc 
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -link-with=icc -T=$(THREADS) -o mtc++ mtc++.o -L/home/gup/k333770/kernels/mt -lcilkmt -lirc -lsvml

mt2c++:	mt-2cbuf-cilk.upc libcilkmt.so
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -T=$(THREADS) -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -o mt2c++.o -c mt-2cbuf-cilk.upc
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -link-with=icc -T=$(THREADS) -o mt2c++ mt2c++.o -L/home/gup/k333770/kernels/mt -lcilkmt -lirc -lsvml

mt3c++:	mt-3cbuf-cilk.upc libcilkmt.so
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -T=$(THREADS) -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -o mt3c++.o -c mt-3cbuf-cilk.upc
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -link-with=icc -T=$(THREADS) -o mt3c++ mt3c++.o -L/home/gup/k333770/kernels/mt -lcilkmt -lirc -lsvml 

run++:	mtc++ 
	( export UPC_CILK_NWORKERS=$(CILKTHREADS);\
	  $(UPCR) $(QUIET) -network=$(NETWORK) --shared-heap=$(HEAPS) -c $(PN) -n $(THREADS) mtc++ )

run2++:	mt2c++ 
	( export UPC_CILK_NWORKERS=$(CILKTHREADS);\
	$(UPCR) $(QUIET) -network=$(NETWORK) --shared-heap=$(HEAPS) -c $(PN) -n $(THREADS) mt2c++ )

run3++:	mt3c++
	( export UPC_CILK_NWORKERS=$(CILKTHREADS);\
	$(UPCR) $(QUIET)  -network=$(NETWORK) --shared-heap=$(HEAPS) -c $(PN) -n $(THREADS) mt3c++ )

cilk+-transpose.o:
	$(CC) $(TFLAGS) -axSSE4.2 -o cilk+-transpose.o -c cilk+-transpose.c -fPIC -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT)

mtc+:	mt-cbuf-cilk+.upc cilk+-transpose.o
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -T=$(THREADS) -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -c -o mtc+.o  mt-cbuf-cilk+.upc 
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -link-with=icc -T=$(THREADS) -o mtc+ mtc+.o cilk+-transpose.o -lcilkrts

run+:	mtc+
	(export UPC_CILK_NWORKERS=$(CILKTHREADS);\
	$(UPCR) $(QUIET) -network=$(NETWORK) --shared-heap=$(HEAPS) -c $(PN) -n $(THREADS) mtc+ )

mt2c+:	mt-2cbuf-cilk+.upc cilk+-transpose.o
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -T=$(THREADS) -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -c -o mt2c+.o  mt-2cbuf-cilk+.upc
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -link-with=icc -T=$(THREADS) -o mt2c+ mt2c+.o cilk+-transpose.o -lcilkrts 

mt3c+:	mt-3cbuf-cilk+.upc cilk+-transpose.o
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -T=$(THREADS) -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -c -o mt3c+.o  mt-3cbuf-cilk+.upc
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -link-with=icc -T=$(THREADS) -o mt3c+ mt3c+.o cilk+-transpose.o -lcilkrts 

run2+:	mt2c+ 
	(export UPC_CILK_NWORKERS=$(CILKTHREADS);\
	$(UPCR) $(QUIET) -network=$(NETWORK) --shared-heap=$(HEAPS) -c $(PN) -n $(THREADS) mt2c+ )

run3+:	mt3c+ 
	( export UPC_CILK_NWORKERS=$(CILKTHREADS);\
	$(UPCR) $(QUIET)  -network=$(NETWORK) --shared-heap=$(HEAPS) -c $(PN) -n $(THREADS) mt3c+ )

perf2+:	mt2c+ 
	(export UPC_CILK_NWORKERS=$(CILKTHREADS);\
	$(PPWR) --output=mt2c-4.par --comm-stats --trace $(UPCR) $(QUIET) -network=$(NETWORK) --shared-heap=$(HEAPS) -c $(PN) -n $(THREADS) mt2c+ )


mpi2c+: mt-2cbuf-cilk+.c cilk+-transpose.o
	$(CC) -O3 $(TFLAGS)  -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -c -o mpi2c+.o mt-2cbuf-cilk+.c -fpic
	$(CC) -O3 $(TFLAGS)  -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -o mpi2c+  mpi2c+.o cilk+-transpose.o -lmpi -lcilkrts -fpic

mpi2k+: mt-2kk2buf-cilk+.c cilk+-transpose.o
	$(CC) -O3 $(TFLAGS)  -DMATN=$(MATSIZE)                  -DPSQRT=$(PSQRT) -c -o mpi2k+.o mt-2kk2buf-cilk+.c -fpic
	$(CC) -O3 $(TFLAGS)  -DMATN=$(MATSIZE)                  -DPSQRT=$(PSQRT) -o mpi2k+  mpi2k+.o cilk+-transpose.o -lmpi -lcilkrts -fpic

runmpi2k+: mpi2k+
	(export CILK_NWORKERS=$(CILKTHREADS);\
	mpirun $(NODES) mpi2k+ )

runmpi2+: mpi2c+
	(export CILK_NWORKERS=$(CILKTHREADS);\
	mpirun $(NODES) mpi2c+ )

omt3c+:	oopl-mt-3cbuf-cilk+.upc cilk+-transpose.o
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -T=$(THREADS) -DMATN=$(MATSIZE) -DBSIZE=$(BSIZE) -DPSQRT=$(PSQRT) -c -o omt3c+.o oopl-mt-3cbuf-cilk+.upc
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -link-with=icc -T=$(THREADS) -o omt3c+ omt3c+.o cilk+-transpose.o -lcilkrts 

orun3+:	omt3c+ 
	(export UPC_CILK_NWORKERS=$(CILKTHREADS);\
	$(UPCR) $(QUIET) -network=$(NETWORK) --shared-heap=$(HEAPS) -c $(PN) -n $(THREADS) omt3c+ )



mtae:	mt-ae-cilk.upc cilk+-transpose.o
	$(UPCC) -network=$(NETWORK) $(CFLAGS) -T=$(THREADS) -DMATN=$(MATSIZE) -c -o mtae.o  mt-ae-cilk.upc 
	$(UPCC) -network=$(NETWORK) -link-with=icc -T=$(THREADS) -o mtae mtae.o cilk+-transpose.o -lcilkrts  

runae:	mtae 
	(export UPC_CILK_NWORKERS=$(CILKTHREADS);\
	$(UPCR) $(QUIET) -network=$(NETWORK) --shared-heap=$(HEAPS)  -n $(THREADS) mtae )

mtsingle: mt.c
	icc -O3  -o mtsingle -DMATN=$(MATSIZE) -DLEAFSIZE=$(LS) mt.c -lcilkrts -DMKL_ILP64 -mkl=parallel $(MLIBS)


mtcilk++: libcilkmt.so
	$(CC) -O3 -o mtcilk++ -DMATN=$(MATSIZE) -DBSIZE=1 -DPSQRT=$(PSQRT) -DLEAFSIZE=$(LS) -DCILKTHREADS=8 -D__UPC__  mt-cilk.c -L/home/ica/k339970/mpolak/mt2 -lcilkmt -lirc -lsvml

mtcilk: mt.cilk
	$(CILKC) -O3 -o mtcilk -DMATN=$(MATSIZE) -DLEAFSIZE=$(LS) mt.cilk $(CILKDEFS) $(CILKINCL)


.PHONY: clean
clean: 
	rm -f mt libcilkmt.so mtsingle mtcilk mtcilk++ mtc++ mt2c++ mt3c++ mtc+ *.o mtae mt2c+ mt3c+ mpi2c+ mpi2k+ omt3c+ *.o
