prefix ?= /usr/local

bin  = sysconf
conf = sysconfrc.conf.example

readme  = README
license = COPYING
contrib = CONTRIBUTORS

.PHONY: default all install

default: all
all: install

install: $(bin) $(conf)
	install -v -m 755 -D $(bin) $(prefix)/bin/$(bin)
	install -v -m 644 -D $(conf) $(prefix)/share/sysconf/$(conf)

	install -v -m 644 -D $(readme) $(prefix)/share/doc/sysconf/$(readme)
	install -v -m 644 -D $(license) $(prefix)/share/doc/sysconf/$(license)
	install -v -m 644 -D $(contrib) $(prefix)/share/doc/sysconf/$(contrib)
