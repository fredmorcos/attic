BIN = pet

HC ?= ghc
HC_FLAGS ?= -Wall --make -O2

HPC ?= hpc
HPC_FLAGS ?= --exclude=Main

HLINT ?= hlint
HLINT_FLAGS ?= --color --cross

SRC = ListUtils.hs StringUtils.hs Pretty.hs   \
			Help.hs Parser.hs Command.hs Expense.hs \
			Main.hs
OBJ = $(SRC:.hs=.o)
IFS = $(SRC:.hs=.hi)

TEST_SRC =
TEST_OBJ = $(TEST_SRC:.hs=.o)
TEST_IFS = $(TEST_SRC:.hs=.hi)
TEST_BIN = $(TEST_SRC:.hs=)
TEST_TIX = $(TEST_SRC:.hs=.tix)

HTMLS = $(SRC:.hs=.hs.html) $(TEST_SRC:.hs=.hs.html)

.PHONY: default all clean cleantix test lint

default: all
all: $(BIN)

$(TEST_BIN): $(TEST_SRC)
	@for i in $(TEST_BIN); do \
		$(HC) $(HC_FLAGS) -fhpc $$i.hs -o $$i; \
	done

lint:
	$(HLINT) . $(HLINT_FLAGS)

test: cleantix $(TEST_BIN) lint
	@for i in $(TEST_BIN); do \
		./$$i; \
	done
	$(HPC) report $(HPC_FLAGS) $(TEST_TIX)
	$(HPC) markup $(HPC_FLAGS) $(TEST_TIX)

$(BIN): $(SRC)
	$(HC) $(HC_FLAGS) $(SRC) -o $(BIN)

cleantix:
	rm -rf $(TEST_TIX)

clean:
	rm -rf $(OBJ) $(IFS) $(BIN) \
		$(TEST_BIN) $(TEST_OBJ) $(TEST_IFS) $(TEST_TIX) \
		$(HTMLS) hpc_index*
