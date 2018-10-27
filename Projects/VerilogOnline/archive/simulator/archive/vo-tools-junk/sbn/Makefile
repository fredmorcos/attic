OBJS = main.meta mem.meta

all: sbn

%.meta: %.tmp
	cat $< | json-meta > $@ 2>/dev/null

%.tmp: %.fig
	fig-json $< > $@ 2>/dev/null

sbn: $(OBJS)
