progname  = powerutils
prefix   := /usr/local
modules  := cpufreq

# User config ends here, do not change anything below unless you know
# what you're doing

modfiles   = $(foreach module,$(modules),$(progname)-$(module))
extrafiles = README.md LICENSE AUTHORS
config     = powerutils.conf  

install_exec = install -m 755 -D
install_norm = install -m 644 -D

.PHONY: default all clean install

default: all
all: $(modfiles)

install: $(modfiles) $(progname) $(extra) $(config)
	$(install_exec) $(progname) $(prefix)/bin/$(progname)

	for f in $(modfiles); do                      \
		$(install_exec) $$f $(prefix)/bin/$$f \
	done

	for f in $(extrafiles); do                                  \
		$(install_norm) $$f $(prefix)/share/$(progname)/$$f \
	done

	$(install_norm) $(config) $(prefix)/share/$(progname)/$$f
