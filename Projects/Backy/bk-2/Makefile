CFLAGS += -D_DEFAULT_SOURCE
CFLAGS += -D_FILE_OFFSET_BITS=64
CFLAGS += -D_FORTIFY_SOURCE=2

CFLAGS += -std=c99
CFLAGS += -W
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -pedantic
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
CFLAGS += -Wcast-qual
CFLAGS += -Wcast-align
CFLAGS += -Wfloat-equal
CFLAGS += -Wold-style-definition
CFLAGS += -Wredundant-decls
CFLAGS += -Wmissing-braces

ifeq ($(REL),yes)
CFLAGS  += -DNDEBUG
CFLAGS  += -O2
CFLAGS  += -flto
LDFLAGS += -flto
else
CFLAGS += -DDEBUG
CFLAGS += -O
CFLAGS += -fno-omit-frame-pointer
CFLAGS += -ggdb
endif

LDFLAGS += -lm
LDFLAGS += -lpthread

NAME     = bk
BINS     = bkc bkd
BIN_OBJS = bkc.o bkd.o

OBJS  = fs.o
OBJS += net.o

SRCS := $(OBJS:.o=.c)
SRCS += $(BIN_OBJS:.o=.c)
HDRS := $(OBJS:.o=.h)

.PHONY: all clean install cppcheck

all: $(BINS)

%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c -o $@ $<

$(BINS): %: %.o $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	$(RM) $(BINS) $(BIN_OBJS) $(OBJS)

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

CPPCFLAGS += --std=c99
CPCCFLAGS += --std=posix
CPPCFLAGS += --report-progress
CPPCFLAGS += --inline-suppr
CPCCFLAGS += --enable=all
CPPCFLAGS += --suppress=missingIncludeSystem
CPPCFLAGS += --suppress=readdirCalled
CPPCFLAGS += --suppress=getpwnamCalled
CPPCFLAGS += --suppress=unmatchedSuppression

cppcheck: $(SRCS)
	cppcheck $(CPPCFLAGS) $(SRCS)

show-cflags:
	@echo "$(CFLAGS)"

show-cxxflags:
	@echo "$(CXXFLAGS)"
