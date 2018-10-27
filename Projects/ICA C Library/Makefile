export CC ?= gcc

ifeq ($(CC),cc)
	CC = gcc
endif

CFLAGS_CHK = -Wall -Werror
CFLAGS_OPT = -O3

ifeq ($(CC),gcc)
	CFLAGS_CHK += -pedantic
endif

export CFLAGS = $(CFLAGS_CHK) $(CFLAGS_OPT)

export LDFLAGS = -lrt

ifeq ($(CC),gcc)
	LDFLAGS += -lm
endif

# SUBDIRS and clean target
SUBDIRS = lib tests
.PHONY: clean subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

clean:
	for dir in $(SUBDIRS); do $(MAKE) clean -C $$dir; done