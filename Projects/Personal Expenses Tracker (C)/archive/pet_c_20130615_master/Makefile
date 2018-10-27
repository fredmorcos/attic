bin = pet

src = main.c
hdr =
obj = $(src:.c=.o)

cc := gcc
cflags := -pedantic -Wall -std=c99
ldflags =

cflags += $(shell pkg-config --cflags glib-2.0)
ldflags += $(shell pkg-config --libs glib-2.0)

ifeq ($(debug),yes)
	dbgflags := -g -DDEBUG
else
	dbgflags :=
endif

.PHONY: all clean default check-syntax

default: all
all: $(bin)

$(bin): $(obj)
	$(cc) $(dbgflags) $(ldflags) $^ -o $@

%.o: %.c $(hdr)
	$(cc) $(dbgflags) $(cflags) -c $< -o $@

clean:
	rm -rf $(bin) $(obj)

check-syntax:
	$(cc) $(cflags) -fsyntax-only $(CHK_SOURCES)
