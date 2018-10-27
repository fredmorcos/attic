CC := gcc

JSON_CFLAGS = $(shell pkg-config --cflags yajl)
JSON_LDFLAGS = $(shell pkg-config --libs yajl)

CFLAGS = -Wall -Werror -pedantic -std=c99 $(JSON_CFLAGS)
LDFLAGS = $(JSON_LDFLAGS)

OBJS = main.o
BIN = cas

.PHONY: default all clean check

default: all
all: $(BIN)

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@

$(BIN): $(OBJS)
	$(CC) $^ -o $@

check:
	cppcheck .

clean:
	rm -rf $(BIN) $(OBJS)
