BIN = pet
SRC = Main.hs Extras/List.hs Extras/Maybe.hs Extras/String.hs
IFS = $(SRC:.hs=.hi)
OBJ = $(SRC:.hs=.o)

.PHONY: default all lint clean

default: all
all: $(BIN)

$(BIN): $(SRC)
	ghc -Wall --make $< -o $@

lint: $(SRC)
	hlint $(SRC)

clean:
	rm -rf $(BIN) $(IFS) $(OBJ)
