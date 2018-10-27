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

#================================================

# upc and cilk stuff
# show_help
THREADS_PER_NODE ?= 1

# UPC stuff
# show_help
NETWORK ?= ibv
# show_help
UPC_THREADS ?= 1

UPC_FLAGS   = -network=$(NETWORK) -T=$(UPC_THREADS) -Wc,"$(CFLAGS)"
UPC_LDFLAGS = -network=$(NETWORK) -T=$(UPC_THREADS) -link-with=$(CC)

# upc runtime stuff
# show_help
UPC_NODES ?= $(shell hvlt_fastest $(UPC_THREADS) $(THREADS_PER_NODE))
# show_help
UPC_HEAP ?= 1G

# upc run command
UPC_RUN_ENV    = GASNET_SSH_SERVERS="$(UPC_NODES)" UPC_SHARED_HEAP_SIZE=$(UPC_HEAP)
UPC_RUN_ARGS_2 = -c $(THREADS_PER_NODE) -N $(UPC_THREADS)
UPC_RUN_ARGS   = $(UPC_RUN_ARGS_1) $(UPC_RUN_ARGS_2)

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

# upc programs
upc-pointers-arrays: DEFINES += -D__UPC
upc-pointers-arrays: upc-pointers-arrays.o
	$(UPCC) $(UPC_LDFLAGS) -o $@ $^

# .PHONY: run-upc-pointers-arrays
run-upc-pointers-arrays: upc-pointers-arrays
	$(UPC_RUN_ENV) $(UPCR) $(UPC_RUN_ARGS) ./$<

#================================================

# make stuff
.PHONY: default all clean help

default: all
all: run-upc-pointers-arrays

clean:
	rm -f *.o *.trans.* core
	rm -f upc-pointers-arrays

# shows all lines following "# show_help"
# use for showing all build and run properties
# the @ avoid echoing this rule's command before executing it
help:
	@grep -A 1 "\# show_help" Makefile | grep -Pv "grep|--|# "