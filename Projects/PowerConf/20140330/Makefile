BIN  = powerconf
SRC  = map.c file.c cpu-freq.c tunable.c
HDR  = $(SRC:.c=.h) config.h debug.h
OBJ  = $(SRC:.c=.o)

CC = gcc -Wall -pedantic -std=c99

DEBUG ?= no

ifeq ($(DEBUG),yes)
	CC += -g
endif

.PHONY: default all clean
default: all
all: $(BIN)

%.o: %.c $(HDR)
	$(CC) -c $< -o $@

$(BIN): $(OBJ) main.o $(HDR)
	$(CC) $(OBJ) main.o -o $@

clean:
	rm -rf $(BIN) $(OBJ) main.o *~
