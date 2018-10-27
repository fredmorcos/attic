BIN = pet
SRC = main.c
OBJ = $(SRC:.c=.o)

CFLAGS += -pedantic -Wall -Wextra -std=c99

.PHONY: all clean default check-syntax

default: all
all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(BIN) $(OBJ)

check-syntax:
	$(CC) $(CFLAGS) -fsyntax-only $(CHK_SOURCES)
