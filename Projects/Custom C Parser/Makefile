CFLAGS += -D_DEFAULT_SOURCE
CFLAGS += -D_FILE_OFFSET_BITS=64
CFLAGS += -D_FORTIFY_SOURCE=2

CFLAGS += -std=c99
CFLAGS += -pedantic
CFLAGS += -W
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -Wpedantic
CFLAGS += -Wwrite-strings
CFLAGS += -Wincompatible-pointer-types
CFLAGS += -Wbad-function-cast
CFLAGS += -Wconversion
CFLAGS += -Wuninitialized
CFLAGS += -Winit-self
CFLAGS += -Wundef
CFLAGS += -Wswitch-enum
CFLAGS += -Wswitch-bool
CFLAGS += -Wcast-align
CFLAGS += -Wfloat-equal
CFLAGS += -Wold-style-definition
CFLAGS += -Wredundant-decls
CFLAGS += -Wmissing-braces
CFLAGS += -fstrict-aliasing
CFLAGS += -Wstrict-overflow

ifeq ($(REL),yes)
CFLAGS  += -DNDEBUG
CFLAGS  += -O2
CFLAGS  += -fomit-frame-pointer
CFLAGS  += -flto
LDFLAGS += -flto
else
CFLAGS += -DDEBUG
CFLAGS += -O
CFLAGS += -fno-omit-frame-pointer
CFLAGS += -g
CFLAGS += -ggdb
endif

LDFLAGS += -lm

NAME  = rfpfind
BIN   = rfpfind
OBJS  = parser.o
OBJS += lexer.o
SRCS := $(OBJS:.o=.c)
HDRS := $(OBJS:.o=.h)

.PHONY: all clean install cppcheck

all: $(BIN)

%.o: %.c $(HDRS)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(BIN): $(OBJS)
	$(CC) $(LDFLAGS) $(LOADLIBES) $(LDLIBS) $^ -o $@

clean:
	$(RM) $(BIN) $(OBJS)

CPPCFLAGS += --inline-suppr
CPPCFLAGS += --report-progress
CPPCFLAGS += --std=c99
CPPCFLAGS += --std=posix
CPPCFLAGS += --enable=all
CPPCFLAGS += --suppress=missingIncludeSystem
CPPCFLAGS += --suppress=unmatchedSuppression

cppcheck: $(SRCS)
	cppcheck $(CPPCFLAGS) $(SRCS)

show-cflags:
	@echo "$(CFLAGS)"
