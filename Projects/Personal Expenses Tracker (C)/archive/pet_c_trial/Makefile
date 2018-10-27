SRC = main.c
OBJ = $(SRC:.c=.o)
BIN = pet

CC := cc
CFLAGS += -Wall -Werror -pedantic-errors -std=c99

LINTING ?= no
LINT := splint
LINTFLAGS += -exportlocal

ifeq ($(DEBUG), yes)
	CFLAGS += -O0 -ggdb
else
	CFLAGS += -O2
endif

.PHONY: default all clean lint

default: all
all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) $^ -o $@

%.o: %.c
ifeq ($(LINTING), yes)
		$(LINT) $(LINTFLAGS) $<
endif

	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ) $(BIN)
