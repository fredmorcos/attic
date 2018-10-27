CFLAGS += -D_DEFAULT_SOURCE -D_FILE_OFFSET_BITS=64 -D_FORTIFY_SOURCE
CFLAGS += -std=c99 -pedantic -W -Wall -Wextra -Wpedantic

ifeq ($(REL),yes)
CFLAGS += -DNDEBUG -O2
else
CFLAGS += -DDEBUG -O -g
endif

BIN  = fed
SRC := fed.c
OBJ += $(SRC:.c=.o)

.PHONY: all clean install cppcheck

all: $(BIN)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

$(BIN): %: %.o $(OBJ)
	$(CC) -o $@ $^ $(LDFLAGS)

clean:
	$(RM) $(BIN) $(OBJ)

.ONESHELL:
install: all
ifeq ($(DEST),)
	$(warning DEST is not set, will assume DEST=/usr/local)
	$(eval DEST = /usr/local)
endif
	$(eval SHARE_DIR = $(DEST)/share/$(BIN))
	$(eval BIN_DIR   = $(DEST)/bin)
	mkdir -p $(SHARE_DIR)
	mkdir -p $(BIN_DIR)
	install -m 644 LICENSE $(SHARE_DIR)
	install -m 755 $(BIN) $(BIN_DIR)

CPPC_FLAGS += --std=c99 --std=posix --enable=all
CPPC_FLAGS += --suppress=missingIncludeSystem
CPPC_FLAGS += --suppress=readdirCalled
CPPC_FLAGS += --suppress=unmatchedSuppression

cppcheck: $(SRC)
	cppcheck $(CPPC_FLAGS) $(SRC)
