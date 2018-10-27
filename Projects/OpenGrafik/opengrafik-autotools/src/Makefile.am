AUTOMAKE_OPTIONS = foreign
bin_PROGRAMS = opengrafik 
opengrafik_SOURCES = main.cc

SUBDIRS = canvas ui

INCLUDES = $(DEPS_CFLAGS)
LIBS = $(DEPS_LIBS)
LDADD = $(DEPS_LIBS) 

LDADD += canvas/libogcanvas.a ui/libogui.a

