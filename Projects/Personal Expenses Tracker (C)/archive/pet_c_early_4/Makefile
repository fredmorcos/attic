BIN = pet

SRC = opts.c main.c
OBJ = $(SRC:.c=.o)

CFLAGS += -Wall -Wextra -std=c11 -pedantic

ifeq ($(debug), yes)
	CFLAGS += -g
endif

.PHONY: default all clean check-syntax

default: all
all: $(BIN)

.c.o:
	$(CC) $(CFLAGS) -c $<

$(BIN): $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	rm -rf $(BIN) $(OBJ)

check-syntax:
	$(CC) $(CFLAGS) -fsyntax-only $(CHK_SOURCES)
