# Note to Self: Using repetitive $(src) and $(obj) instead of $^ in
# target commands because BSD make does not support the latter. Also,
# this will not mess up anything when I or someone else adds other
# different dependencies to the rule.

bin = pet
src = main.c
obj = $(src:.c=.o)

.PHONY: clean depend all default

default: all
all: $(bin)

$(bin): $(obj) depend
	gcc -o $@ $(obj)

depend: $(src)
	gcc -E -MM $(src) > .depend

clean:
	rm -rf $(bin) $(obj) .depend
