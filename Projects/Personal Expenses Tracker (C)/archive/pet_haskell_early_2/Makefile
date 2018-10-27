SRC = Expense.hs Index.hs Parser.hs Main.hs
OBJ = $(SRC:.hs=.o)
IFS = $(SRC:.hs=.hi)
BIN = pet

HC ?= ghc
HFLAGS ?=

ifeq ($(DEBUG),yes)
	DFLAGS += -debug
endif

ifeq ($(COVERAGE),yes)
	DFLAGS += -cov
endif

.PHONY: default all clean

default: all
all: $(BIN)

$(BIN): $(SRC)
	$(HC) $(HFLAGS) --make $^ -o $@

clean:
	rm -rf $(BIN) $(OBJ) $(IFS)
