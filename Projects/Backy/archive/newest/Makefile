CSTD = c99

CFLAGS += -D_DEFAULT_SOURCE
CFLAGS += -D_BSD_SOURCE
CFLAGS += -D_FILE_OFFSET_BITS=64
CFLAGS += -D_FORTIFY_SOURCE=2

CFLAGS += -std=$(CSTD)
CFLAGS += -pedantic
CFLAGS += -W
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -Wpedantic
CFLAGS += -Wmissing-include-dirs
CFLAGS += -Wmain
CFLAGS += -Wunreachable-code
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
CFLAGS += -Wunused-function
CFLAGS += -Wunused-label
CFLAGS += -Wunused-parameter
CFLAGS += -Wunused-variable
CFLAGS += -Wunused-const-variable
CFLAGS += -Wundef
CFLAGS += -Wswitch
CFLAGS += -Wswitch-enum
CFLAGS += -Wswitch-default
CFLAGS += -Wmissing-format-attribute
CFLAGS += -Wattributes
CFLAGS += -Wignored-attributes
CFLAGS += -Waggregate-return
CFLAGS += -Wvla
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

# CFLAGS += -fms-extensions

ifeq ($(FATAL_ERRORS),yes)
CFLAGS += -Wfatal-errors
endif

ifeq ($(FATAL_WARNINGS),yes)
CFLAGS += -Werror
endif

ifeq ($(CC),clang)
CFLAGS += -Wdocumentation
CFLAGS += -Weverything
CFLAGS += -Wdangling-else
CFLAGS += -Wstrncat-size
CFLAGS += -Wint-conversion
CFLAGS += -Wint-to-pointer-cast
CFLAGS += -Wno-padded
CFLAGS += -Wno-covered-switch-default
CFLAGS += -Wno-c++98-compat
CFLAGS += -Wno-c++98-compat-pedantic
CFLAGS += -Wno-disabled-macro-expansion
CFLAGS += -Wno-ignored-attributes
CFLAGS += -Wno-unknown-attributes

# CFLAGS += -Wno-c99-compat
# CFLAGS += -Wno-microsoft
# CFLAGS += -Wno-documentation-unknown-command
# CFLAGS += -Wno-exit-time-destructors
else
CFLAGS += -Wunused-but-set-variable
CFLAGS += -Wunused-but-set-parameter
CFLAGS += -Wsuggest-attribute=const
CFLAGS += -Wsuggest-attribute=pure
CFLAGS += -Wsuggest-attribute=noreturn
CFLAGS += -Wsuggest-attribute=format
CFLAGS += -Wno-c90-c99-compat

# CFLAGS += -Wno-c99-c11-compat
# CFLAGS += -fplan9-extensions
endif

ifeq ($(RELEASE),yes)
CFLAGS += -DNDEBUG
CFLAGS += -O3
CFLAGS += -fomit-frame-pointer

CFLAGS  += -flto
LDFLAGS += -flto
else
CFLAGS += -DDEBUG
CFLAGS += -O
CFLAGS += -fno-omit-frame-pointer
CFLAGS += -g

CFLAGS += -fstack-protector
CFLAGS += -fstack-protector-all
CFLAGS += -fstack-protector-strong
CFLAGS += -Wstack-protector
CFLAGS += -fstack-check

CFLAGS += -ftrapv
endif

ifeq ($(PROFILING),yes)
CFLAGS  += -pg
LDFLAGS += -pg
endif

ifeq ($(PCHECKS),yes)
CFLAGS += -mmpx
CFLAGS += -fcheck-pointer-bounds
CFLAGS += -fchkp-check-incomplete-type
CFLAGS += -fchkp-narrow-bounds
CFLAGS += -fchkp-narrow-to-innermost-array
CFLAGS += -fchkp-first-field-has-own-bounds
CFLAGS += -fchkp-narrow-to-innermost-array
CFLAGS += -fchkp-optimize
CFLAGS += -fchkp-use-fast-string-functions
CFLAGS += -fchkp-use-nochk-string-functions
CFLAGS += -fchkp-use-static-bounds
CFLAGS += -fchkp-use-static-const-bounds
CFLAGS += -fchkp-treat-zero-dynamic-size-as-infinite
CFLAGS += -fchkp-check-read
CFLAGS += -fchkp-check-write
CFLAGS += -fchkp-store-bounds
CFLAGS += -fchkp-instrument-calls
CFLAGS += -fchkp-instrument-marked-only
CFLAGS += -fchkp-use-wrappers
endif

ifeq ($(SANITIZE_UNDEFINED),yes)
SFLAGS += -fsanitize=undefined
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
endif

# Hint: Use the package hardening-wrapper for gcc and clang.

# Hint: Use  the runtime  environment variable  MALLOC_PERTURB_=1 when
# testing. Supported by glibc.

ifeq ($(SANITIZE_MEMORY),yes)
SFLAGS += -fsanitize=memory
endif

ifeq ($(SANITIZE_LEAK),yes)
SFLAGS += -fsanitize=leak
endif

ifeq ($(SANITIZE_ADDRESS),yes)
SFLAGS += -fsanitize=address
endif

ifeq ($(SANITIZE_THREAD),yes)
SFLAGS += -fsanitize=thread
endif

ifeq ($(SANITIZE_INTEGER),yes)
SFLAGS += -fsanitize=integer
SFLAGS += -fsanitize=unsigned-integer-overflow
endif

ifeq ($(SANITIZE_CFI),yes)
SFLAGS += -fsanitize=cfi-cast-strict
SFLAGS += -fsanitize=cfi-nvcall
SFLAGS += -flto
endif

# ifeq ($(SANITIZE_STACK),yes)
# SFLAGS += -fsanitize=safe-stack
# endif

# ifeq ($(SANITIZE_COVERAGE_BB),yes)
# SFLAGS += -fsanitize-coverage=bb
# else ifeq ($(SANITIZE_COVERAGE_EDGE),yes)
# SFLAGS += -fsanitize-coverage=edge
# else ifeq ($(SANITIZE_COVERAGE_FUNC),yes)
# SFLAGS += -fsanitize-coverage=func
# endif

# Disable omitting of frame pointer when sanitizations are enabled
ifneq ($(SFLAGS),)
CFLAGS += -fno-omit-frame-pointer
endif

# Add the sanitization flags to CFLAGS and LDFLAGS
CFLAGS  += $(SFLAGS)
LDFLAGS += $(SFLAGS)

# Libraries
LDFLAGS += -lpthread
LDFLAGS += -lm

NAME     = backy
BIN_OBJS = bkc.o bkd.o
BINS     = $(BIN_OBJS:.o=)

OBJS  = ai.o
OBJS += fs.o
OBJS += lsdir.o
OBJS += mem.o
OBJS += net.o
OBJS += num.o
OBJS += parse.o
OBJS += stat.o
OBJS += str.o
OBJS += thread.o
OBJS += time.o
OBJS += vec.o

SRCS  = $(OBJS:.o=.c)
SRCS += $(BIN_OBJS:.o=.c)
HDRS  = $(OBJS:.o=.h)

.PHONY: all depend clean install cppcheck check-syntax
.PRECIOUS: %.o

all: $(BINS)

depend:
	$(CC) -E -MM $(SRCS) > .depend

%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c -o $@ $<

$(BINS): %: %.o $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	$(RM) $(BIN_OBJS) $(BINS) $(OBJS)

cleanall: clean
	$(RM) GPATH GRTAGS GTAGS compile_commands.json .depend

.ONESHELL:
install: all
ifeq ($(DEST),)
	$(warning DEST is not set, will assume DEST=/usr/local)
	$(eval DEST = /usr/local)
endif
	$(eval SHARE_DIR = $(DEST)/share/$(NAME))
	$(eval BIN_DIR   = $(DEST)/bin)
	mkdir -p $(SHARE_DIR)
	mkdir -p $(BIN_DIR)
	install -m 644 LICENSE $(SHARE_DIR)
	install -m 755 $(BINS) $(BIN_DIR)

CPPC_FLAGS += --std=$(CSTD)
CPPC_FLAGS += --std=posix
CPPC_FLAGS += --enable=all
CPPC_FLAGS += --suppress=missingIncludeSystem
CPPC_FLAGS += --suppress=readdirCalled
CPPC_FLAGS += --suppress=unmatchedSuppression

cppcheck: $(SRCS)
	cppcheck $(CPPC_FLAGS) $(SRCS)

check-syntax:
	gcc   $(CFLAGS) -fsyntax-only $(CHK_SOURCES)
	clang $(CFLAGS) -fsyntax-only $(CHK_SOURCES)
