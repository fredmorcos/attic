OBJS = main.machine

all: sbn

%.machine: %.json %.meta
	vofc $< > $@

%.meta: %.fig
	fig-json < $^ > $@

sbn: $(OBJS)
