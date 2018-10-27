# Hint: Also use MALLOC_PERTURB_=1 when testing

ifeq ($(SAN_CHECK),yes)
SFLAGS += -mmpx -fcheck-pointer-bounds
else
ifeq ($(SAN_THREAD),yes)
SFLAGS += -fsanitize=thread
else
SFLAGS += -fsanitize=address
endif

SFLAGS += -fsanitize=leak
SFLAGS += -fsanitize=undefined

# Should be under -fsanitize=undefined, but I am not sure
SFLAGS += -fsanitize=shift
SFLAGS += -fsanitize=integer-divide-by-zero
SFLAGS += -fsanitize=unreachable
SFLAGS += -fsanitize=vla-bound
SFLAGS += -fsanitize=null
SFLAGS += -fsanitize=return
SFLAGS += -fsanitize=signed-integer-overflow
SFLAGS += -fsanitize=bounds
SFLAGS += -fsanitize=alignment
SFLAGS += -fsanitize=object-size
SFLAGS += -fsanitize=float-divide-by-zero
SFLAGS += -fsanitize=float-cast-overflow
SFLAGS += -fsanitize=nonnull-attribute
SFLAGS += -fsanitize=returns-nonnull-attribute
SFLAGS += -fsanitize=bool
SFLAGS += -fsanitize=enum
SFLAGS += -fsanitize=vptr

SFLAGS += -fno-omit-frame-pointer
SFLAGS += -fstack-protector
SFLAGS += -fstack-protector-all
SFLAGS += -fstack-protector-strong
SFLAGS += -Wstack-protector

SFLAGS_CLANG += -fsanitize=integer
SFLAGS_CLANG += -fsanitize=memory
SFLAGS_CLANG += -fsanitize=safe-stack
SFLAGS_CLANG += -fsanitize=cfi-nvcall
SFLAGS_CLANG += -fsanitize=cfi-cast-strict
SFLAGS_CLANG += -fsanitize-coverage=func
SFLAGS_CLANG += -fsanitize-coverage=bb
SFLAGS_CLANG += -fsanitize-coverage=edg
endif

CFLAGS += -D_DEFAULT_SOURCE
CFLAGS += -std=c11
CFLAGS += -W -Wall -Wextra -pedantic -Wpedantic
CFLAGS += -Wwrite-strings
CFLAGS += -Wpointer-arith
CFLAGS += -Wincompatible-pointer-types
CFLAGS += -Wbad-function-cast
CFLAGS += -Winline
CFLAGS += -Wsign-conversion
CFLAGS += -Wconversion
CFLAGS += -Wuninitialized
CFLAGS += -Winit-self
CFLAGS += -Wunused
CFLAGS += -Wunused-result
CFLAGS += -Wunused-value
CFLAGS += -Wundef
CFLAGS += -Wswitch
CFLAGS += -Wswitch-enum
CFLAGS += -Wswitch-default
CFLAGS += -Wmissing-format-attribute
CFLAGS += -Wshadow
CFLAGS += -Wcast-align
CFLAGS += -Wcast-qual
CFLAGS += -Wfloat-conversion
CFLAGS += -Wfloat-equal
CFLAGS += -Wmissing-prototypes
CFLAGS += -Wstrict-prototypes
CFLAGS += -Wmissing-declarations
CFLAGS += -Wold-style-definition
CFLAGS += -Wnested-externs
CFLAGS += -Wredundant-decls
CFLAGS += -Wunknown-pragmas
CFLAGS += -Wmissing-braces
CFLAGS += -Wmissing-field-initializers

CFLAGS += -fno-common
CFLAGS += -ftree-vectorize
CFLAGS += -fstrict-aliasing
CFLAGS += -Wstrict-aliasing
CFLAGS += -fstrict-overflow
CFLAGS += -Wstrict-overflow=5

CFLAGS_GCC += -Wsuggest-attribute=const
CFLAGS_GCC += -Wsuggest-attribute=pure
CFLAGS_GCC += -Wsuggest-attribute=noreturn
CFLAGS_GCC += -Wsuggest-attribute=format
CFLAGS_GCC += -Wno-c90-c99-compat
CFLAGS_GCC += -Wno-c99-c11-compat

CFLAGS_CLANG += -Weverything
CFLAGS_CLANG += -Wno-padded
CFLAGS_CLANG += -Wno-covered-switch-default
CFLAGS_CLANG += -Wno-c++98-compat
CFLAGS_CLANG += -Wno-c++98-compat-pedantic
CFLAGS_CLANG += -Wno-c99-compat
CFLAGS_CLANG += -Wno-disabled-macro-expansion
CFLAGS_CLANG += -Wdocumentation
# CFLAGS_CLANG += -Wno-documentation-unknown-command
# CFLAGS_CLANG += -Wno-exit-time-destructors

ifeq ($(CC),gcc)
	CFLAGS += $(CFLAGS_GCC)
endif

ifeq ($(CC),cc)
	CFLAGS += $(CFLAGS_GCC)
endif

ifeq ($(CC),clang)
	CFLAGS += $(CFLAGS_CLANG)
	SFLAGS += $(SFLAGS_CLANG)
endif

ifeq ($(SAN),yes)
	CFLAGS  += $(SFLAGS)
	LDFLAGS += $(SFLAGS)
endif

ifeq ($(PROF),yes)
	CFLAGS  += -pg
	LDFLAGS += -pg
endif

ifeq ($(REL),yes)
	CFLAGS += -O3 -DNDEBUG -fomit-frame-pointer
else
	CFLAGS += -g -DDEBUG
endif

CFLAGS += -D_FILE_OFFSET_BITS=64

LIBS     = libbsd
CFLAGS  += $(shell pkg-config --cflags $(LIBS))
LDFLAGS += $(shell pkg-config --libs   $(LIBS))
LDFLAGS += -lpthread
LDFLAGS += -lm

OBJS  = obj.o
OBJS += str.o
OBJS += con.o
HDRS  = $(OBJS:.o=.h)

BIN_OBJS  = obj-test.o
BIN_OBJS += str-test.o
BIN_OBJS += con-test.o
BINS      = $(BIN_OBJS:.o=)

SRCS  = $(OBJS:.o=.c)
SRCS += $(BIN_OBJS:.o=.c)

.PHONY: clean all

all: $(BINS)

%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -o $@ -c $<

$(BINS): %: %.o $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@

cppcheck: $(SRCS) $(HDRS)
	cppcheck --enable=all --std=posix --std=c11 $(SRCS)

clean:
	rm -f $(BINS) $(BIN_OBJS) $(OBJS)
