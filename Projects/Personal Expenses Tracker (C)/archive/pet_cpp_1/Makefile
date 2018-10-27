BIN = pet

SRC = opts.cc help.cc cmd.cc main.cc
OBJ = $(SRC:.cc=.o)

CXXFLAGS += -Wall -Wextra -std=c++11 -pedantic

.PHONY: default all clean

default: all
all: $(BIN)

.cc.o:
	$(CXX) $(CXXFLAGS) -c $<

$(BIN): $(OBJ)
	$(CXX) $(LDFLAGS) $^ -o $@

clean:
	rm -rf $(BIN) $(OBJ)
