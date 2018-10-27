CC ?= gcc
CFLAGS += -Wall -Wextra -pedantic -std=c11 -g

BINS = test_ctx_mm
SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)

VAL ?= valgrind
VFLAGS += --leak-check=full --show-leak-kinds=all -v

test_ctx_mm: test_ctx_mm.o ctx_mm.o

test: $(BINS)
	$(foreach b,$(BINS),$(VAL) $(VFLAGS) ./$(b))

clean:
	rm -rf $(BINS) $(OBJS)
