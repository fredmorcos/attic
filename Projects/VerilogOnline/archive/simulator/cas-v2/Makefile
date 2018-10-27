SUBDIRS = sbn
SRC = cas.py cac.py fig2json.py

.PHONY: default all subdirs $(SUBDIRS) clean test

default: test all
all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

test: $(SRC)
	pylint -f colorized $^

clean:
	$(MAKE) clean -C $(SUBDIRS)
