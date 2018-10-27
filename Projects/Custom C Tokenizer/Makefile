CC        ?= gcc
CFLAGS    ?= -pedantic -Wall -Werror -O3 -std=c99
LDFLAGS   ?= -Wl,--hash-style=gnu -Wl,--as-needed

SRCS       = $(shell find src -iname "*.c")
OBJS       = $(SRCS:.c=.o)

TARGET     = plafoon
DEST      := /usr/local

all: $(TARGET)

debug: DEBUG_CFLAGS = -g -DDEBUG
debug: all

.c.o:
	$(CC) $(CFLAGS) $(DEBUG_CFLAGS) -c -o $@ $^

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(DEBUG_CFLAGS) $(LDFLAGS) -o $@ $^

clean:
	rm -rf src/*.o $(TARGET)

install: all
	mkdir -p $(DEST)/bin/
	cp $(TARGET) $(DEST)/bin/.
