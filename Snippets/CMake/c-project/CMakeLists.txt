cmake_minimum_required(VERSION 2.6)
project(grafer)

find_package(PkgConfig)
pkg_check_modules(XLIB x11)
pkg_check_modules(XEXT xext)
pkg_check_modules(CAIRO cairo)

add_subdirectory(src)

