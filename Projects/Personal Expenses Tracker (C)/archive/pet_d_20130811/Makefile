SRC = expense.d document.d parser.d logging.d info.d help.d main.d
OBJ = $(SRC:.d=.o)
COV = $(SRC:.d=.lst)
BIN = pet

DC ?= dmd
DFLAGS ?=
LFLAGS ?=

DOCSDIR ?= code_docs

ifeq ($(DEBUG),yes)
	DFLAGS += -debug -g
	LFLAGS += $(DFLAGS)
endif

ifeq ($(COVERAGE),yes)
	DFLAGS += -cov
	LFLAGS += $(DFLAGS)
endif

ifeq ($(DOCS),yes)
	DFLAGS += -D -Dd$(DOCSDIR)
endif

.PHONY: default all clean

default: all
all: $(BIN)

$(DOCSDIR):
	mkdir -p $(DOCSDIR)

$(BIN): $(OBJ)
	$(DC) $(LFLAGS) $^ -of$@

%.o: %.d $(DOCSDIR)
	$(DC) $(DFLAGS) -c $<

clean:
	rm -rf $(BIN) $(OBJ) $(COV) $(DOCSDIR)
