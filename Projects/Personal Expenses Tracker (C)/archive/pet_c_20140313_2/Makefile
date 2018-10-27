BIN = pet
SRC = main.c
OBJ = $(SRC:.c=.o)

CFLAGS += -Wall -Wextra

.PHONY: default all clean
default: all
all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(BIN) $(OBJ)
