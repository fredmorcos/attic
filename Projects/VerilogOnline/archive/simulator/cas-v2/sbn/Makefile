TARGET = sbn.machine

sbn.machine: sbn.json sbn.meta sbn.v
	../cac.py $< > $@

sbn.meta: sbn.fig
	../fig2json.py < $< > $@

clean:
	rm -rf *.meta $(TARGET)
