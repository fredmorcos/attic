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
CFLAGS  += -O3
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
LDFLAGS += -lpthread
LDFLAGS += -lcrypto

NAME = shatag
BINS = shatag
OBJS = shatag.o

SRCS := $(OBJS:.o=.c)

.PHONY: all clean install cppcheck

all: $(BINS)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

$(BINS): %: %.o $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	$(RM) $(BINS) $(OBJS)

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
	install -m 755 bkc $(BIN_DIR)
	install -m 755 bkd $(BIN_DIR)

CPPC_FLAGS += --std=c99 --std=posix --enable=all
CPPC_FLAGS += --suppress=missingIncludeSystem
CPPC_FLAGS += --suppress=readdirCalled
CPPC_FLAGS += --suppress=getpwnamCalled
CPPC_FLAGS += --suppress=unmatchedSuppression

cppcheck: $(SRCS)
	cppcheck $(CPPC_FLAGS) $(SRCS)
