include $(GNUSTEP_MAKEFILES)/common.make

LIBRARY_NAME = libXKit

libXKit_OBJCFLAGS += -std=c99
libXKit_CFLAGS += -std=c99

libXKit_OBJC_FILES = $(wildcard *.m)
libXKit_C_FILES = $(wildcard *.c)
libXKit_HEADER_FILES_INSTALL_DIR = XKit
libXKit_HEADER_FILES = $(wildcard *.h)

# libXKit_TOOL_LIBS    += -lX11
# libXKit_INCLUDE_DIRS += 
# libXKit_LIB_DIRS     += 

include $(GNUSTEP_MAKEFILES)/library.make

