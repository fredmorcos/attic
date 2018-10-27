BIN = pet
SRC = Main.hs Extras/List.hs Extras/Maybe.hs Extras/String.hs
IFS = $(SRC:.hs=.hi)
OBJ = $(SRC:.hs=.o)

.PHONY: default all clean

default: all
all: $(BIN)

$(BIN): $(SRC)
	ghc -Wall --make $< -o $@

clean:
	rm -rf $(BIN) $(IFS) $(OBJ)
