CC      ?= gcc
CFLAGS  ?= -pedantic -Wall -Werror -std=c99
LDFLAGS ?= -Wl,--hash-style=gnu -Wl,--as-needed
SRCS     = main.c
TARGETS  = fbcalc
DEST    := /usr/local

all: $(TARGETS)

par.o: par.tab.c
	$(CC) par.tab.c -c -o $@

lex.o: lex.yy.c par.tab.c
	$(CC) lex.yy.c -c -o $@

lex.yy.c: lex.l
	flex lex.l

par.tab.c:
	bison -d par.y

debug: CFLAGS += -g -DDEBUG
debug: all

fbcalc: lex.o par.o $(SRCS)
	$(CC) $(CFLAGS) -c -o main.o $(SRCS)
	$(CC) $(LDFLAGS) lex.o par.o main.o -o $@

clean:
	rm -rf $(TARGETS) *.o *.yy.* *.tab.*

.PHONY: clean

install: all
	mkdir -p $(DEST)/bin/
	cp $(TARGETS) $(DEST)/bin/.
