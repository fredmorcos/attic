SRCS=$(wildcard *.c)
BINS=$(SRCS:.c=)

all: $(BINS)

clean:
	$(RM) $(BINS)

%_clang: %.c
	$(CC) $(CFLAGS) -Wall -Wextra -fsanitize=address -g $< -o $@
