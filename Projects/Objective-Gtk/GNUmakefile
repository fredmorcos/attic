include $(GNUSTEP_MAKEFILES)/common.make

LIBRARY_NAME = Gtk
$(LIBRARY_NAME)_OBJC_FILES = $(wildcard *.m)
$(LIBRARY_NAME)_HEADER_FILES = $(wildcard *.h)
$(LIBRARY_NAME)_HEADER_FILES_INSTALL_DIR = $(LIBRARY_NAME)

$(LIBRARY_NAME)_LIBRARY_LIBS += `pkg-config --libs gtk+-2.0`
$(LIBRARY_NAME)_INCLUDE_DIRS += `pkg-config --cflags gtk+-2.0`

include $(GNUSTEP_MAKEFILES)/library.make
