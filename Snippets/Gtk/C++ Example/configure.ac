AC_INIT(src/main.cpp)
AM_INIT_AUTOMAKE(gnome-sidebar,0.1)
AM_CONFIG_HEADER(config.h)
AC_PROG_CC
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_LANG_CPLUSPLUS

AC_ARG_ENABLE(bling, AC_HELP_STRING([--enable-bling], [Enable bling, needs a compositing manager. unstable, default=disabled]), bling=$enableval, bling=no)
AC_ARG_ENABLE(stdout, AC_HELP_STRING([--disable-stdout], [Disable stdout messages for tracing. default=enabled]), stdout=$enableval, stdout=yes)
AC_ARG_ENABLE(debug, AC_HELP_STRING([--disable-debug], [Disable debugging symbols. default=enabled]), debug=$enableval, debug=yes)

if test "$bling" = yes; then
#	PKG_CHECK_MODULES(GTKSOURCEVIEW, gtksourceview-2.0 >= 2.0)
	AC_DEFINE(BLING_ENABLED,,[bling])
fi

if test "$stdout" = yes; then
#	PKG_CHECK_MODULES(GTKSOURCEVIEW, gtksourceview-2.0 >= 2.0)
	AC_DEFINE(STDOUT_ENABLED,,[stdout])
fi

if test "$debug" = yes; then
#	PKG_CHECK_MODULES(GTKSOURCEVIEW, gtksourceview-2.0 >= 2.0)
	AC_DEFINE(DEBUG_ENABLED,,[debug])
	CFLAGS="$CFLAGS -g -Wall -Werror"
	CXXFLAGS="$CXXFLAGS -g -Wall -Werror"
fi

AM_CONDITIONAL(DEBUG_ENABLED, test "x$debug" = "xyes")

PKG_CHECK_MODULES(DEPS, gtkmm-2.4 >= 2.4 cairomm-1.0 >= 1.0)
AC_SUBST(DEPS_CFLAGS)
AC_SUBST(DEPS_LIBS)
AC_OUTPUT(Makefile \
	  src/Makefile)

