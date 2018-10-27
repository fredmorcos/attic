HC     ?= ghc
HFLAGS ?=

HFLAGS += -Wall --make

BIN = rffe_query

SRC   = Main.hs \
	SimpleParse.hs \
	PrologParse.hs \
	ArgsHandler.hs \
	QueryHandler.hs \
	Printing.hs

OBJ = $(SRC:.hs=.o)
IFS = $(SRC:.hs=.hi)

.PHONY: default all clean

default: all

all: $(BIN)

$(BIN): $(SRC)
	$(HC) $(HFLAGS) $(SRC) -o $(BIN)

clean:
	rm -rf $(BIN) $(OBJ) $(IFS)
