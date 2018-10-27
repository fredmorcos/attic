bin = pet

src = main.c
obj = $(src:.c=.o)

cc := gcc
cflags := -pedantic -Wall -std=c99

ifeq ($(debug),yes)
	dbgflags := -g
else
	dbgflags :=
endif

.PHONY: all clean default check-syntax

default: all
all: $(bin)

$(bin): $(obj)
	$(cc) $(dbgflags) $^ -pthread -o $@

%.o: %.c
	$(cc) $(dbgflags) $(cflags) -c $< -o $@

clean:
	rm -rf $(bin) $(obj)

check-syntax:
	$(cc) $(cflags) -fsyntax-only $(CHK_SOURCES)
