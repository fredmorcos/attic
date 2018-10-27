cmake_minimum_required(VERSION 2.6)

set(PROJ_NAME objective-gtk)

project(${PROJ_NAME})

add_definitions(-Wall -Werror)
set(CMAKE_EXE_LINKER_FLAGS -lobjc)

find_package(PkgConfig)
pkg_check_modules(GTK gtk+-2.0)

subdirs(src)

