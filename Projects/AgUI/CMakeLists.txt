# Fred Morcos <fred.morcos@gmail.com>

project(opengrafik)

find_package(PkgConfig)

pkg_check_modules(GTKMM gtkmm-2.4)
pkg_check_modules(CAIROMM cairomm-1.0)
pkg_check_modules(SIGCPP sigc++-2.0)

subdirs(src)

