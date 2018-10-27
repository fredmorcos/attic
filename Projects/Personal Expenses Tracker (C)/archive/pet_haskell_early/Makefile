SRC = Expense.hs Index.hs Parser.hs Main.hs
OBJ = $(SRC:.hs=.o)
IFS = $(SRC:.hs=.hi)
BIN = pet

HC ?= ghc
HFLAGS ?= -O -threaded -Wall

HL ?= hlint

ifeq ($(DEVEL),yes)
	DEBUG=yes
	COVERAGE=yes
	LINT=yes
endif

ifeq ($(DEBUG),yes)
	DFLAGS += -debug
endif

ifeq ($(COVERAGE),yes)
	DFLAGS += -cov
endif

.PHONY: default all clean lint

default: all
all: $(BIN)

$(BIN): $(SRC)
ifeq ($(LINT),yes)
	$(HL) $^
endif
	$(HC) $(HFLAGS) --make $^ -o $@

clean:
	rm -rf $(BIN) $(OBJ) $(IFS)
