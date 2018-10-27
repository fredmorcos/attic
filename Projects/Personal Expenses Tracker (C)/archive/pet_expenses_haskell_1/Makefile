HC   := ghc
BIN  := expenses
SRC  := Main.hs DateHelper.hs Functions.hs Parser.hs Errors.hs
OBJS := $(SRC:.hs=.o)
IFS  := $(SRC:.hs=.hi)

.PHONY: clean
default: all
all: $(BIN)

$(BIN): $(SRC)
	@ghc --make $^ -o $@

clean:
	rm -rf $(BIN) $(OBJS) $(IFS)
