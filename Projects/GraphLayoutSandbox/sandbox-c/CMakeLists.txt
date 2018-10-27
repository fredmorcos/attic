cmake_minimum_required(VERSION 2.8)

project(sandbox)

find_package(PkgConfig)
pkg_check_modules(GTK gtk+-2.0)

# ${GTK_LIBRARY_DIRS}
# ${GTK_INCLUDE_DIRS}
# ${GTK_LIBRARIES}

subdirs(src)

