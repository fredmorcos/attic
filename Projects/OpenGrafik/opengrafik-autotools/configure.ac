AC_INIT(src/main.cc)
AM_INIT_AUTOMAKE(opengrafik,0.1)
AM_CONFIG_HEADER(config.h)
AC_PROG_CC
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_LANG_CPLUSPLUS

# AC_ARG_ENABLE(buttons, AC_HELP_STRING([--enable-buttons], [Enable the extra buttons (default=yes)]), buttons=$enableval, buttons=no)

# if test "$buttons" = yes; then
#	PKG_CHECK_MODULES(GTKSOURCEVIEW, gtksourceview-2.0 >= 2.0)
#	AC_DEFINE(ENABLE_BUTTONS,,[Enable the extra buttons])
# fi

# AM_CONDITIONAL(ENABLE_BUTTONS, test "x$buttons" = "xyes")

PKG_CHECK_MODULES(GTKMM, gtkmm-2.4 >= 2.4)
PKG_CHECK_MODULES(CAIROMM, cairomm-1.0 >= 1.0)
PKG_CHECK_MODULES(LIBGLADEMM, libglademm-2.4 >= 2.4)
AC_SUBST(DEPS_CFLAGS)
AC_SUBST(DEPS_LIBS)
AC_OUTPUT(Makefile \
	  src/Makefile \
	  src/canvas/Makefile \
	  src/ui/Makefile)

