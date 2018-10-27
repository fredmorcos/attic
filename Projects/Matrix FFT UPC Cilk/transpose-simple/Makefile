# NOTES:
# 1- will only work properly with icc
# 2- the `# show_help' comment before a line L is used to grep out L
#    when running `make help'

# compilers
UPCC ?= upcc
UPCR ?= upcrun
CC   ?= icc

# c and cilk compiler flags
CFLAGS  += -O3 -funroll-loops -Wall -Werror
LDFLAGS += -lrt

#================================================

# upc and cilk stuff
# show_help
THREADS_PER_NODE ?= 1

# UPC stuff
# show_help
NETWORK ?= ibv
# show_help
UPC_THREADS ?= 1

UPC_TFLAGS  = -T=$(UPC_THREADS)
UPC_FLAGS   = -network=$(NETWORK) $(UPC_TFLAGS) -Wc,"$(CFLAGS)"
UPC_LDFLAGS = -link-with=$(CC) $(UPC_TFLAGS) $(LDFLAGS)

# upc runtime stuff
# show_help
UPC_NODES ?= "$(shell hvlt_fastest $(UPC_THREADS) $(THREADS_PER_NODE))"
# show_help
UPC_HEAP ?= 1G

# upc run command
UPC_RUN_ENV  = GASNET_SSH_SERVERS=$(UPC_NODES) UPC_SHARED_HEAP_SIZE=$(UPC_HEAP)
UPC_RUN_ARGS = -shared-heap=$(UPC_HEAP) -c $(THREADS_PER_NODE) -N $(UPC_THREADS)

# files
OBJS = gettime.o

#================================================

# debug stuff
# show_help
DEBUG ?= no

ifeq ($(DEBUG),yes)
DEFINES += -DDEBUG
CFLAGS += -g
endif

#================================================

# do matrix check or not
# show_help
DO_CHECK ?= yes
# show_help
SHOW_MATRIX ?= yes

ifeq ($(DO_CHECK),yes)
DEFINES += -DDO_CHECK
endif

ifeq ($(SHOW_MATRIX),yes)
DEFINES += -DSHOW_MATRIX
endif

#================================================

# matrix properties
# show_help
MAT_WIDTH ?= 10
# show_help
MAT_HEIGHT ?= 10
# show_help
LEAF_SIZE_I ?= 2
# show_help
LEAF_SIZE_J ?= 2

DEFINES += -DMAT_WIDTH=$(MAT_WIDTH) -DMAT_HEIGHT=$(MAT_HEIGHT)
DEFINES += -DLEAF_SIZE_I=$(LEAF_SIZE_I) -DLEAF_SIZE_J=$(LEAF_SIZE_J)

#================================================

# compile c files
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

# simplest c only transpose
transpose: CFLAGS += -cilk-serialize
transpose: transpose.o $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^

# .PHONY: run-transpose
run-transpose: transpose
	./$<

# cilk transpose
cilk-transpose: DEFINES += -D__CILK
cilk-transpose: LDFLAGS += -lcilkrts
cilk-transpose: transpose.o $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^

# .PHONY: run-cilk-transpose
run-cilk-transpose: cilk-transpose
	./$<

# upc programs
upc-transpose: DEFINES += -D__UPC
upc-transpose: OBJS += transpose-body.o
upc-transpose: upc-transpose.o $(OBJS)
	$(UPCC) $(UPC_LDFLAGS) -o $@ $^

# .PHONY: run-upc-transpose
run-upc-transpose: upc-transpose
	$(UPC_RUN_ENV) $(UPCR) $(UPC_RUN_ARGS) ./$<

# upc-cilk transpose
upc-cilk-transpose: DEFINES += -D__UPC -D__CILK
upc-cilk-transpose: LDFLAGS += -lcilkrts
upc-cilk-transpose: transpose.o upc-transpose.o $(OBJS)
	$(UPCC) $(UPC_LDFLAGS) -o $@ $^

# .PHONY: run-upc-cilk-transpose
run-upc-cilk-transpose: upc-cilk-transpose
	$(UPC_RUN_ENV) $(UPCR) $(UPC_RUN_ARGS) ./$<

#================================================

# make stuff
.PHONY: default all clean help

default: all
all: run-transpose run-cilk-transpose run-upc-transpose run-upc-cilk-transpose

clean:
	rm -f *.o *.trans.*
	rm -f transpose cilk-transpose upc-transpose upc-cilk-transpose

# shows all lines following "# show_help"
# use for showing all build and run properties
# the @ avoid echoing this rule's command before executing it
help:
	@grep -A 1 "\# show_help" Makefile | grep -Pv "grep|--|# "