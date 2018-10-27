BIN = markr
SRC = Templates.hs Settings.hs Main.hs
OBJ = $(SRC:.hs=.o)
IFS = $(SRC:.hs=.hi)

GHC ?= ghc
GHC_FLAGS ?= -Wall -Werror -O2

.PHONY: default all clean run
default: all
all: $(BIN)

$(BIN): $(SRC)
	$(GHC) $(GHC_FLAGS) $^ -o $@

run: $(BIN)
	./$(BIN)

clean:
	rm -rf $(OBJ) $(IFS) $(BIN)
