bin = pet
core = pet.core

srcs = cmdline.c help.c file.c expense.c parser.c main.c
hdrs = cmdline.h help.h file.h expense.h parser.h
objs = $(srcs:.c=.o)

CFLAGS += -Wall -Wextra -Wshadow -pedantic

# lint = lint
# splint = splint

ifeq ($(debug), yes)
	CFLAGS += -g
endif

.PHONY: default all clean check-syntax # lint splint
default: all
all: $(bin)

%.o: %.c $(hdrs)
	$(CC) $(CFLAGS) -c $< -o $@

$(bin): $(objs)
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	rm -rf $(bin) $(objs) $(core)

# lint:
#	$(lint) -a -aa -c -e -h -r -x $(srcs)

# splint:
#	$(splint) $(srcs)

check-syntax:
	$(cc) $(cflags) -fsyntax-only $(CHK_SOURCES)
