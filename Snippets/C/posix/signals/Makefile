CC ?= gcc
VG ?= valgrind

VGFLAGS = --leak-check=full --show-reachable=yes -v

SRC = main.c
OBJ = $(SRC:.c=.o)

BIN = powerutils

.PHONY: clean default all
default: all
all: $(BIN)

%.o: %.c
	$(CC) -c $^

$(BIN): $(OBJ)
	$(CC) $^ -o $@

clean:
	rm -rf $(OBJ) $(BIN) *~

leakcheck: $(BIN)
	$(VG) $(VGFLAGS) ./$<
