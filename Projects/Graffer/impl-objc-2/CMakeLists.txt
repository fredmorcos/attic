cmake_minimum_required(VERSION 2.6)

project(grafer CXX)

add_definitions(-Wall -Werror)

find_package(PkgConfig)
# pkg_check_modules(GTK gtk+-2.0)
pkg_check_modules(GTK objective-gtk)
pkg_check_modules(GLIB glib-2.0)
pkg_check_modules(CAIRO cairo)

# message(${GTK_LIBRARY_DIRS})
# message(${GTK_INCLUDE_DIRS})
# message(${GTK_LIBRARIES})

subdirs(src)

