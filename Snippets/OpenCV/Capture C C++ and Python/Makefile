BIN = hand-tracking
SRC = main.cc
OBJ = $(SRC:.cc=.o)

CC = g++
CFLAGS = -Wall -pedantic

.PHONY: default all clean

default: all
all: $(BIN)

# opencv
CFLAGS += $(shell pkg-config --cflags opencv)
LDFLAGS = $(shell pkg-config --libs opencv)

%.o: %.cc
	@echo "[CC] $< -> $@"
	@$(CC) $(CFLAGS) -c $< -o $@

$(BIN): $(OBJ)
	@echo "[LD] $@"
	@$(CC) $(LDFLAGS) $^ -o $@

clean:
	rm -rf $(OBJ) $(BIN)
