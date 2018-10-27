cmake_minimum_required(VERSION 2.6)

set(PROJ_NAME imageman)
set(LIB_NAME objective-cv)

project(${PROJ_NAME})

# add_definitions(-Wall -Werror)
set(CMAKE_EXE_LINKER_FLAGS -lobjc)

find_package(PkgConfig)

subdirs(lib src)

