hc = ghc
src = Main.hs
obj = $(src:.hs=.o)
ifs = $(src:.hs=.hi)

bin = Expenses

.PHONY: clean default all
default: all
all: $(bin)

%.o: %.hs
	$(hc) -c $<

$(bin): $(obj)
	$(hc) $^ -o $@

clean:
	rm -rf $(bin) $(ifs) $(obj) *~