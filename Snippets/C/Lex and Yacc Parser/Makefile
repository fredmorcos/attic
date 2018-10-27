CC        ?= gcc
CFLAGS    ?= -pedantic -Wall -Werror -O3 -std=c99
LDFLAGS   ?= -Wl,--hash-style=gnu -Wl,--as-needed

SRCS       = $(shell find src -iname "*.c" -and -not \
	                      -iname "*.yy.c" -and -not \
	                      -iname "*.tab.c")
OBJS       = $(SRCS:.c=.o)

all: hllvm

debug: DEBUG_CFLAGS = -g -DDEBUG
debug: all

.c.o:
	$(CC) $(CFLAGS) $(DEBUG_CFLAGS) -c -o $@ $^

src/lex.yy.c: src/lex.l
	flex -o $@ $^

src/lex.o: src/lex.yy.c src/par.tab.c
	$(CC) $(DEBUG_CFLAGS) -c -o $@ src/lex.yy.c

src/par.tab.c: src/par.y
	bison -d $^ -o $@

src/par.o: src/par.tab.c
	$(CC) $(DEBUG_CFLAGS) -c -o $@ $^

hllvm: src/lex.o src/par.o $(OBJS)
	$(CC) $(CFLAGS) $(DEBUG_CFLAGS) $(LDFLAGS) -o $@ $^

clean:
	rm -rf src/*.o hllvm src/*.tab.* src/*.yy.c
