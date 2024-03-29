# Use CC=gcc and CC=clang
# Use MALLOC_PERTURB_=1 runtime env-var when testing (glibc).

include Makefile.flags
include Makefile.flags-clang
include Makefile.flags-gcc

STD = c11

CFLAGS += -std=$(STD)

ifneq (,$(findstring clang,$(CC)))
CFLAGS += -Wdocumentation
CFLAGS += -Weverything
CFLAGS += -Wdangling-else
CFLAGS += -Wstrncat-size
CFLAGS += -Wint-conversion
CFLAGS += -Wint-to-pointer-cast
CFLAGS += -Wno-padded
CFLAGS += -Wno-covered-switch-default
CFLAGS += -Wno-disabled-macro-expansion
CFLAGS += -Wno-ignored-attributes
CFLAGS += -Wno-unknown-attributes
else
CFLAGS += -Wunused-but-set-variable
CFLAGS += -Wunused-but-set-parameter
CFLAGS += -Wno-c90-c99-compat
endif

CFLAGS_DEBUG += -DDEBUG
CFLAGS_DEBUG += -fno-omit-frame-pointer
CFLAGS_DEBUG += -g
CFLAGS_DEBUG += -fstack-protector
CFLAGS_DEBUG += -fstack-protector-all
CFLAGS_DEBUG += -fstack-protector-strong
CFLAGS_DEBUG += -Wstack-protector
CFLAGS_DEBUG += -fstack-check
CFLAGS_DEBUG += -ftrapv

ifeq (,$(findstring clang,$(CC)))
CFLAGS_DEBUG += -fvar-tracking
CFLAGS_DEBUG += -fvar-tracking-assignments
endif

CFLAGS_RELEASE += -DNDEBUG
CFLAGS_RELEASE += -O3
CFLAGS_RELEASE += -fomit-frame-pointer
CFLAGS_RELEASE += -flto

ifeq (,$(findstring clang,$(CC)))
CFLAGS_PTRCHECK += -mmpx
CFLAGS_PTRCHECK += -fcheck-pointer-bounds
CFLAGS_PTRCHECK += -fchkp-check-incomplete-type
CFLAGS_PTRCHECK += -fchkp-narrow-bounds
CFLAGS_PTRCHECK += -fchkp-narrow-to-innermost-array
CFLAGS_PTRCHECK += -fchkp-first-field-has-own-bounds
CFLAGS_PTRCHECK += -fchkp-narrow-to-innermost-array
CFLAGS_PTRCHECK += -fchkp-optimize
CFLAGS_PTRCHECK += -fchkp-use-fast-string-functions
CFLAGS_PTRCHECK += -fchkp-use-nochk-string-functions
CFLAGS_PTRCHECK += -fchkp-use-static-bounds
CFLAGS_PTRCHECK += -fchkp-use-static-const-bounds
CFLAGS_PTRCHECK += -fchkp-treat-zero-dynamic-size-as-infinite
CFLAGS_PTRCHECK += -fchkp-check-read
CFLAGS_PTRCHECK += -fchkp-check-write
CFLAGS_PTRCHECK += -fchkp-store-bounds
CFLAGS_PTRCHECK += -fchkp-instrument-calls
CFLAGS_PTRCHECK += -fchkp-instrument-marked-only
CFLAGS_PTRCHECK += -fchkp-use-wrappers
endif

CFLAGS_SAN += -fsanitize=undefined
CFLAGS_SAN += -fsanitize=integer-divide-by-zero
CFLAGS_SAN += -fsanitize=unreachable
CFLAGS_SAN += -fsanitize=vla-bound
CFLAGS_SAN += -fsanitize=null
CFLAGS_SAN += -fsanitize=return
CFLAGS_SAN += -fsanitize=signed-integer-overflow
CFLAGS_SAN += -fsanitize=bounds
CFLAGS_SAN += -fsanitize=bool
CFLAGS_SAN += -fsanitize=builtin

CFLAGS_SAN_ADDR += -fsanitize=address

ifeq (,$(findstring clang,$(CC)))
CFLAGS_SAN_ADDR += -fsanitize=pointer-compare
CFLAGS_SAN_ADDR += -fsanitize=pointer-subtract
endif

ifneq (,$(findstring clang,$(CC)))
CFLAGS_SAN_MEM += -fsanitize=memory

CFLAGS_SAN_INT += -fsanitize=integer
CFLAGS_SAN_INT += -fsanitize=unsigned-integer-overflow
endif

CFLAGS_SAN_LEAK   += -fsanitize=leak
CFLAGS_SAN_THREAD += -fsanitize=thread

LDFLAGS_RELEASE += -flto
LDFLAGS         += -lpthread
LDFLAGS         += -lm

# Dependencies
CFLAGS  += -Ihashmap/src/
LDFLAGS += -Lhashmap/build/
LDFLAGS += -lhashmap

SRC   = proj.c
OBJS  = proj.o
OBJS += proj_debug.o
OBJS += proj_ptrcheck.o
OBJS += proj_san.o
OBJS += proj_sanaddr.o
OBJS += proj_sanmem.o
OBJS += proj_sanleak.o
OBJS += proj_santhread.o
OBJS += proj_sanint.o
BINS  = $(OBJS:.o=)

.PHONY: all clean cppcheck
.PRECIOUS: %.o

all: $(BINS)

%.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_RELEASE) -c -o $@ $<

proj: %: %.o
	$(CC) $(LDFLAGS) $(LDFLAGS_RELEASE) $^ -o $@

%_debug.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) -c -o $@ $<

proj_debug: %: %.o
proj_ptrcheck: %: %.o
proj_sanleak: %: %.o
	$(CC) $(LDFLAGS) $^ -o $@

proj_sanint: %: %.o
	$(CC) $(CFLAGS_SAN_INT) $(LDFLAGS) $^ -o $@

proj_san: %: %.o
	$(CC) $(CFLAGS_SAN) $(LDFLAGS) $^ -o $@

proj_sanaddr: %: %.o
	$(CC) $(CFLAGS_SAN_ADDR) $(LDFLAGS) $^ -o $@

proj_santhread: %: %.o
	$(CC) $(CFLAGS_SAN_THREAD) $(LDFLAGS) $^ -o $@

proj_sanmem: %: %.o
	$(CC) $(CFLAGS_SAN_MEM) $(LDFLAGS) $^ -o $@

%_ptrcheck.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(CFLAGS_PTRCHECK) -c -o $@ $<

%_san.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(CFLAGS_SAN) -c -o $@ $<

%_sanaddr.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(CFLAGS_SAN_ADDR) -c -o $@ $<

%_sanmem.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(CFLAGS_SAN_MEM) -c -o $@ $<

%_sanleak.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(CFLAGS_SAN_LEAK) -c -o $@ $<

%_santhread.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(CFLAGS_SAN_THREAD) -c -o $@ $<

%_sanint.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(CFLAGS_SAN_INT) -c -o $@ $<

clean:
	$(RM) $(BINS) $(OBJS)

CPPCHECK_FLAGS += --std=$(STD)
CPPCHECK_FLAGS += --std=posix
CPPCHECK_FLAGS += --enable=all
CPPCHECK_FLAGS += --suppress=missingIncludeSystem

cppcheck: $(SRC)
	cppcheck $(CPPCHECK_FLAGS) $^
