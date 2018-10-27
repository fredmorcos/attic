CP = cp $< $@

ALL = _emacs terminalrc

.PHONY: default all
default: all
all: _emacs terminalrc

_emacs: ~/.emacs
	$(CP)

terminalrc: ~/.config/Terminal/terminalrc
	$(CP)

foo:
	for i in $$(ALL); do
		echo $i
	done
