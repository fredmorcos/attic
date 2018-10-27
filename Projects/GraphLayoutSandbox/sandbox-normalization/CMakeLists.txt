cmake_minimum_required(VERSION 2.6)

project(sandbox CXX)

add_definitions(-Wall -Werror)

find_package(PkgConfig)
pkg_check_modules(GTK gtk+-2.0)
pkg_check_modules(GLIB glib-2.0)
pkg_check_modules(CAIRO cairo)

# ${GTK_LIBRARY_DIRS}
# ${GTK_INCLUDE_DIRS}
# ${GTK_LIBRARIES}

subdirs(src)

