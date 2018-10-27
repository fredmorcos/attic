SRC = main.c
OBJ = $(SRC:.c=.o)
BIN = pet

CC ?= cc
CFLAGS += -Wall -Werror -pedantic -std=c99 -O2

ifeq ($(DEBUG), yes)
	CFLAGS += -ggdb
endif

.PHONY: default all clean

default: all
all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ) $(BIN)
