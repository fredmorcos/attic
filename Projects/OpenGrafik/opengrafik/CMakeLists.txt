# Fred Morcos <fred.morcos@gmail.com>

project(opengrafik)

find_package(PkgConfig)

# Set the GTKMM_INCLUDE_DIRS, GTKMM_LIBRARY_DIRS and GTKMM_LIBRARIES variables
pkg_check_modules(GTKMM gtkmm-2.4)
pkg_check_modules(CAIROMM cairomm-1.0)
pkg_check_modules(GLADEMM libglademm-2.4)
pkg_check_modules(SIGCPP sigc++-2.0)

subdirs(src)

