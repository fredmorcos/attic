HC     ?= ghc
HFLAGS += -Wall --make

BIN       = pet
SRC_MAIN  = Main.hs
SRC_OTHER = \
  Expense.hs \
  CommandLine.hs \
  Parser.hs \
  PetParser.hs \
  Extra/List.hs \
  Extra/String.hs \
  Extra/Tuple.hs

SRC = $(SRC_MAIN) $(SRC_OTHER)
OBJ = $(SRC:.hs=.o)
HIF = $(SRC:.hs=.hi)

TESTS = $(wildcard tests/*)

PREFIX ?= /usr/local
OPT    ?= no

ifeq ($(OPT),yes)
HFLAGS += -O
endif

$(BIN): $(SRC)
	$(HC) $(HFLAGS) $(SRC_MAIN) -o $(BIN)

.PHONY:
install: $(BIN)
	mkdir -p $(PREFIX)/bin/
	cp $(BIN) $(PREFIX)/bin/$(BIN)

test: $(BIN) $(TESTS)
	for t in $(TESTS); do \
		./$(BIN) -T $$t; \
	done

clean:
	rm -rf $(BIN) $(OBJ) $(HIF)
