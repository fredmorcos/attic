SRC = Statistics.hs Help.hs Errors.hs    \
	Extras.hs Commands.hs Expense.hs \
	Pipeline.hs Parser.hs Main.hs
BIN = pet

HC = ghc
HC_FLAGS = --make -Wall -debug -dcore-lint

.PHONY: default all clean

default: all
all: $(BIN)

$(BIN): $(SRC)
	$(HC) $(HC_FLAGS) $^ -o $@

clean:
	rm -rf $(BIN) *~ *.hi *.o
