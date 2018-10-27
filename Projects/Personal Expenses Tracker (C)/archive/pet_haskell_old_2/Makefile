SRC = Main.hs CommandLine.hs Parser.hs \
	Expense.hs Commands.hs ErrorMessages.hs
OBJ = $(SRC:.hs=.o)
IFS = $(SRC:.hs=.hi)
BIN = pet

HC ?= ghc
HCFLAGS += -Wall

# Profiling
ifeq ($(PROF),yes)
	HCFLAGS += -prof
endif

# Threaded Runtime
ifeq ($(THREADED),yes)
	HCFLAGS += -threaded
endif

# Optimization
ifeq ($(OPT),yes)
	HCFLAGS += -O
endif

.PHONY: default all clean lint

default: all
all: $(BIN)

$(BIN): $(SRC)
	$(HC) $(HCFLAGS) --make $< -o $@

clean:
	rm -rf $(OBJ) $(IFS) $(BIN) $(BIN).prof

lint:
	hlint $(SRC)
