# NOTES:
# 1- will only work properly with icc

# compilers
UPCC ?= upcc
UPCR ?= upcrun
CC   ?= icc

# c and cilk compiler flags
CFLAGS  += -O3 -Wall -Werror
LDFLAGS += -lrt

#================================================

# cilk stuff

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
UPC_HEAP ?= 1G

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

# do matrix check or not
DO_CHECK ?= yes
SHOW_MAT ?= yes

ifeq ($(DO_CHECK),yes)
DEFINES += -DDO_CHECK
endif

ifeq ($(SHOW_MAT),yes)
DEFINES += -DSHOW_MAT
endif

#================================================

# matrix properties
MAT_H ?= 10
MAT_W ?= 10
ILS ?= 2
JLS ?= 2

DEFINES += -DMAT_W=$(MAT_W) -DMAT_H=$(MAT_H)
DEFINES += -DILS=$(ILS) -DJLS=$(JLS)

#================================================

# files
OBJS += gettime.o main.o

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
