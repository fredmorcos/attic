HC     ?= ghc
HFLAGS ?=

HFLAGS += -Wall --make

BIN = vcard
SRC = $(wildcard *.hs) \
	$(wildcard SimpleParse/*.hs) \
	$(wildcard SimpleParse/Utils/*.hs) \
	$(wildcard VCard/*.hs)
OBJ = $(SRC:.hs=.o)
IFS = $(SRC:.hs=.hi)
TMP = $(SRC:.hs=.hs~)

.PHONY: default all clean

default: all

all: $(BIN)

$(BIN): $(SRC)
	$(HC) $(HFLAGS) $(SRC) -o $(BIN)

clean:
	rm -rf $(BIN) $(OBJ) $(IFS) $(TMP)
