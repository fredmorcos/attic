HC          = ghc
TARGETS     = figjson
CLEANFILES  = $(TARGETS)
CLEANFILES += $(foreach dir,$(TARGETS),$(wildcard src/$(dir)/*.o))
CLEANFILES += $(foreach dir,$(TARGETS),$(wildcard src/$(dir)/*.hi))

all: $(TARGETS)

%: src/%/Main.hs
	ghc -O -o $@ --make $< -Wall

.PHONY: clean
clean:
	rm -rf $(CLEANFILES)
