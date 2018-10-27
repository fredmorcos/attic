AUTOMAKE_OPTIONS = foreign
bin_PROGRAMS = gnome-sidebar
gnome_sidebar_SOURCES = main.cpp \
			gsb-panel.h \
			gsb-panel.cpp

# if ENABLE_BUTTONS
# SUBDIRS = og-canvas
# endif

INCLUDES = $(DEPS_CFLAGS)
LIBS = $(DEPS_LIBS)
LDADD = $(DEPS_LIBS) 

# if ENABLE_BUTTONS
# LDADD += og-canvas/libogcanvas.a
# endif

if DEBUG_ENABLED
	CFLAGS += -g -Wall -Werror
	CXXFLAGS += -g -Wall -Werror
else
	CFLAGS += -g0
	CXXFLAGS += -g0
endif

