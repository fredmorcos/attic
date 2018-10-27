HC   := ghc
BIN  := Expenses
SRC  := Main.hs
OBJS := $(SRC:.hs=.o)
IFS  := $(SRC:.hs=.hi)

.PHONY: clean
default: all
all: $(BIN)

# %.o: %.hs
# 	$(HC) -c $<

# $(BIN): $(OBJS)
# 	$(HC) $^ -o $@

$(BIN): $(SRC)
	ghc --make -O3 -o $@ $^

clean:
	rm -rf $(BIN) $(OBJS) $(IFS)
