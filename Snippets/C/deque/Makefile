CC      ?= gcc
_CFLAGS  = -pedantic -Wall -Werror -std=c99
CFLAGS   = -O3 $(_CFLAGS)
TARGETS  = deque-test

all: $(TARGETS)

debug: CFLAGS = -g -DDEBUG $(_CFLAGS)
debug: all

.c.o:
	$(CC) $< -c -o $@ $(CFLAGS)

deque-test: deque.o bench.o deque-test.o
	$(CC) -o $@ $^ $(CFLAGS)

clean:
	rm -rf *.o $(TARGETS) *~
