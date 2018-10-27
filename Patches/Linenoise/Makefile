BIN = mlce
SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)

CC ?= gcc
CFLAGS += -Wall -Wextra -pedantic -std=c11
CFLAGS += -g

$(BIN): $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $(BIN)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ) $(BIN)
