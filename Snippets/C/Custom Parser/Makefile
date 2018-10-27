CC      ?= gcc
CFLAGS  ?= -pedantic -Wall -Werror -std=c99
SRCS     = src/*.c
TARGETS  = hllc
DEST    := /usr/local

all: $(TARGETS)

debug: CFLAGS += -g -DDEBUG
debug: all

hllc: $(SRCS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(SRCS)

clean:
	rm -rf $(TARGETS)

.PHONY: clean

install: all
	mkdir -p $(DEST)/bin/
	cp $(TARGETS) $(DEST)/bin/.
