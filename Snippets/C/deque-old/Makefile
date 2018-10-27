CC      ?= gcc
CFLAGS   = -O3 -pedantic -Wall -Werror -std=c99
TARGETS  = array-test list-test deque-test

all: $(TARGETS)

debug: CFLAGS = -g -DDEBUG -std=c99 -Werror -Wall -pedantic
debug: all

.c.o:
	$(CC) $< -c -o $@ $(CFLAGS)

deque-test: bench.o deque.o deque-test.o
	$(CC) -o $@ $^ $(CFLAGS)

array-test: bench.o array.o array-test.o
	$(CC) -o $@ $^ $(CFLAGS)

list-test: bench.o list.o list-test.o
	$(CC) -o $@ $^ $(CFLAGS)

clean:
	rm -rf *.o $(TARGETS) *~
