dnl Process this file with autoconf to produce a configure script.

AC_INIT([lowfat], [0.0.1])
AC_PREFIX_DEFAULT(/usr)
AC_PREREQ([2.5])

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE([1.8])

AC_CONFIG_HEADERS([config.h])

dnl Check for C compiler
AC_LANG([C])
AC_ISC_POSIX

dnl Checks for various programs
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_CXX

dnl Various checks just so we're really sane checking (proposed by AzaToth)
AC_C_VOLATILE

AC_TYPE_MODE_T
AC_TYPE_PID_T

AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STAT

dnl Next four lines is a hack to prevent libtool checking for C++/F77
m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])

dnl pkg-config
PKG_PROG_PKG_CONFIG

dnl GLIB
PKG_CHECK_MODULES([GLIB], [glib-2.0 >= 2.8.0 gobject-2.0 gthread-2.0 gmodule-2.0])
AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)

dnl GTK
PKG_CHECK_MODULES([GTK], [gtk+-2.0 >= 2.8.0])
AC_SUBST(GTK_CFLAGS)
AC_SUBST(GTK_LIBS)

dnl CAIRO
PKG_CHECK_MODULES([CAIRO], [cairo >= 1.2.0])
AC_SUBST(CAIRO_CFLAGS)
AC_SUBST(CAIRO_LIBS)

dnl SDL
AC_MSG_CHECKING(for SDL_CFLAGS)
AC_ARG_WITH(sdl-cflags, [  --with-sdl-cflags=CFLAGS ],
                       [SDL_CFLAGS="$withval"],
                       [SDL_CFLAGS="`sdl-config --cflags`"])
AC_MSG_RESULT($SDL_CFLAGS)

AC_MSG_CHECKING(for SDL_LIBS)
AC_ARG_WITH(sdl-libs, [  --with-sdl-libs=LIBS ],
                     [SDL_LIBS="$withval"],
                     [SDL_LIBS="`sdl-config --libs` -lSDL_image"])
AC_MSG_RESULT($SDL_LIBS)

AC_SUBST(SDL_CFLAGS)
AC_SUBST(SDL_LIBS)

dnl OpenGL 
AC_MSG_CHECKING(for GL_CFLAGS)
AC_ARG_WITH(gl-cflags, [  --with-gl-cflags=CFLAGS ],
                       [GL_CFLAGS="$withval"],
                       [GL_CFLAGS="-I/usr/include"])
AC_MSG_RESULT($GL_CFLAGS)

AC_MSG_CHECKING(for GL_LIBS)
AC_ARG_WITH(gl-libs, [  --with-gl-libs=LIBS ],
                     [GL_LIBS="$withval"],
                     [GL_LIBS="-lGL -lGLU"])
AC_MSG_RESULT($GL_LIBS)

AC_SUBST(GL_CFLAGS)
AC_SUBST(GL_LIBS)

dnl CFLAGS
CFLAGS="$CFLAGS -Wall -Wno-deprecated -pedantic -std=c99 -fno-strict-aliasing -fmessage-length=0 -D_FORTIFY_SOURCE=2"

AC_CONFIG_FILES([
    Makefile
    src/Makefile
])

AC_OUTPUT

