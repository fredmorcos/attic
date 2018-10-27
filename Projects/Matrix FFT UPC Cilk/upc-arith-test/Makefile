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

# vector properties: size, leaf size
VEC_LEN ?= 1000

DEFINES += -DVEC_LEN=$(VEC_LEN)

#================================================

# stuff for minor tweaking
BS     ?= VEC_LEN/NT
RUNS   ?= 3

DEFINES += -DRUNS=$(RUNS) -D"BS=$(BS)"

#================================================

# vector traversal methods
METHOD ?= SIMPLE_TEST
DEFINES += -D$(METHOD)

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

# do vector check and printing or not
DO_CHECK ?= yes
SHOW_VEC ?= yes

ifeq ($(DO_CHECK),yes)
DEFINES += -DDO_CHECK
endif

ifeq ($(SHOW_VEC),yes)
DEFINES += -DSHOW_VEC
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
# OBJS += gettime.o

#================================================

# targets
vector-addition: DEFINES += -D__UPC
vector-addition: $(OBJS) addition-split.o main.o
	$(UPCC) $(UPC_LDFLAGS) -o $@ $^

matrix-addition: DEFINES += -D__UPC
matrix-addition: $(OBJS) main-matrix.o
	$(UPCC) $(UPC_LDFLAGS) -o $@ $^

arith-test: DEFINES += -D__UPC
arith-test: $(OBJS) arith-test.o
	$(UPCC) $(UPC_LDFLAGS) -o $@ $^

run-%: %
	$(UPC_RUN_ENV) $(UPCR) $(UPC_RUN_ARGS) ./$<

#================================================

# make stuff
.PHONY: default all clean

default: all
all: run-vector-addition run-matrix-addition

clean:
	rm -f *.o
	rm -f vector-addition matrix-addition
