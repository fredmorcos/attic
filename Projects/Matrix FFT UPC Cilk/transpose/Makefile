# NOTES:
# 1- will only work properly with icc due to the cilk
#    specifics in there, so maybe with gcc>=4.7 too?

# compilers
UPCC ?= upcc
UPCR ?= upcrun
CC   ?= icc

# c and cilk compiler flags
CFLAGS  += -O3 -funroll-all-loops -Wall -Werror
LDFLAGS += -lrt

#================================================

# matrix properties: size, leaf sizes
MAT_H ?= 10
MAT_W ?= 10
ILS   ?= 2
JLS   ?= 2
GILS  ?= 4
GJLS  ?= 4

DEFINES += -DMAT_W=$(MAT_W) -DMAT_H=$(MAT_H)
DEFINES += -DILS=$(ILS) -DJLS=$(JLS)
DEFINES += -DGILS=$(GILS) -DGJLS=$(GJLS)

#================================================

# stuff for minor tweaking
BS     ?= MAT_W/NT
RUNS   ?= 3

DEFINES += -DRUNS=$(RUNS) -DBS=$(BS)

#================================================

# transpose methods
METHOD ?= simple-for

ifeq ($(METHOD),simple-test)
DEFINES += -DSIMPLE_TEST
else
ifeq ($(METHOD),simple-for)
DEFINES += -DSIMPLE_FOR
else
ifeq ($(METHOD),simple-forall)
DEFINES += -DSIMPLE_FORALL
else
ifeq ($(METHOD),sectioned-forall)
DEFINES += -DSECTIONED_FORALL
else
ifeq ($(METHOD),affinity-forall)
DEFINES += -DAFFINITY_FORALL
else
ifeq ($(METHOD),line-transfer-sync)
DEFINES += -DLINE_TRANSFER_SYNC
else
ifeq ($(METHOD),line-transfer-async)
DEFINES += -DLINE_TRANSFER_ASYNC
else
ifeq ($(METHOD),bulk-transfer-sync)
DEFINES += -DBULK_TRANSFER_SYNC
else
ifeq ($(METHOD),bulk-transfer-async)
DEFINES += -DBULK_TRANSFER_ASYNC
else
ifeq ($(METHOD),permutation)
DEFINES += -DPERMUTATION
else
ifeq ($(METHOD),space-partition)
DEFINES += -DSPACE_PARTITION
else
ifeq ($(METHOD),space-partition-forall-toy)
DEFINES += -DSPACE_PARTITION_FORALL_TOY
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif
endif

#================================================

# Cilk stuff
THREADS_PER_NODE ?= 1

ENABLE_CILK ?= yes

ifeq ($(ENABLE_CILK),yes)
DEFINES += -D__CILK
LDFLAGS += -lcilkrts
else
CFLAGS += -cilk-serialize
endif

#================================================

# UPC stuff
NETWORK ?= ibv
UPC_THREADS ?= 1

UPC_TFLAGS  = -T=$(UPC_THREADS)
UPC_FLAGS   = -network=$(NETWORK) $(UPC_TFLAGS) -Wc,"$(CFLAGS)"
UPC_LDFLAGS = -link-with=$(CC) $(UPC_TFLAGS) $(LDFLAGS)

# upc runtime stuff
UPC_NODES ?= "$(shell hvlt_fastest $(UPC_THREADS) $(THREADS_PER_NODE))"
UPC_HEAP  ?= 1G

# upc run command
UPC_RUN_ENV  = GASNET_SSH_SERVERS=$(UPC_NODES) UPC_SHARED_HEAP_SIZE=$(UPC_HEAP)
UPC_RUN_ARGS = -shared-heap=$(UPC_HEAP) -c $(THREADS_PER_NODE) -N $(UPC_THREADS)

#================================================

# debug stuff
DEBUG ?= no

ifeq ($(DEBUG),yes)
DEFINES += -DDEBUG
CFLAGS += -g
endif

#================================================

# do matrix check and printing or not
DO_CHECK ?= yes
SHOW_MAT ?= yes

ifeq ($(DO_CHECK),yes)
DEFINES += -DDO_CHECK
endif

ifeq ($(SHOW_MAT),yes)
DEFINES += -DSHOW_MAT
endif

#================================================

# compile c files -> object files
%.o: %.c
	$(CC) $(CFLAGS) $(DEFINES) -c -o $@ $^

# transform c -> asm
%.c.trans.s: %.c
	$(CC) $(CFLAGS) $(DEFINES) -S -o $@ $^

# transform upc -> c
%.upc.trans.c: %.upc
	$(UPCC) $(UPC_FLAGS) $(DEFINES) -trans -o $@ $^

# compile upc files
%.o: %.upc
	$(UPCC) $(UPC_FLAGS) $(DEFINES) -c -o $@ $^

#================================================

# files
OBJS += gettime.o main.o

#================================================

# targets
transpose: DEFINES += -D__UPC
transpose: $(OBJS)
	$(UPCC) $(UPC_LDFLAGS) -o $@ $^

run-transpose: transpose
	$(UPC_RUN_ENV) $(UPCR) $(UPC_RUN_ARGS) ./$<

#================================================

# make stuff
.PHONY: default all clean

default: all
all: run-transpose

clean:
	rm -f *.o *.trans.*
	rm -f transpose
