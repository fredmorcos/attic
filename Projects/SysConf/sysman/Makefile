bin = sysman

src = main.c
obj = $(src:.c=.o)

cc := gcc
cflags := -pedantic -Wall -std=c99

.PHONY: all clean default check-syntax

default: all
all: $(bin)

$(bin): $(obj)
	$(cc) $^ -pthread -o $@

%.o: %.c
	$(cc) $(cflags) -c $< -o $@

clean:
	rm -rf $(bin) $(obj)

check-syntax:
	$(cc) $(cflags) -fsyntax-only $(CHK_SOURCES)
