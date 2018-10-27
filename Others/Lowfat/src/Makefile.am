INCLUDES = \
		@SDL_CFLAGS@ \
		@GLIB_CFLAGS@ \
		@GTK_CFLAGS@ \
		@CAIRO_CFLAGS@ \
		@GL_CFLAGS@ \
		-Wall \
		-Wno-deprecated \
		-pedantic \
		-I$(top_srcdir)/include \
		-DIMAGEDIR=\"$(imagedir)\"

bin_PROGRAMS = lowfat

lowfat_LDADD = @SDL_LIBS@ \
		@GLIB_LIBS@ \
		@GTK_LIBS@ \
		@CAIRO_LIBS@ \
		@GL_LIBS@

lowfat_LDFLAGS =

lowfat_SOURCES = \
		action.cpp \
		application.cpp \
		background.cpp \
		color.cpp \
		component.cpp \
		date.cpp \
		gui_observer.cpp \
		image.cpp \
		lfdisplay.cpp \
		lffont.cpp \
		lfwindow.cpp \
		lowfat.cpp \
		main.cpp \
		photo_observer.cpp \
		selection.cpp \
		shared_ptr.cpp \
		texture.cpp \
		vector2.cpp
