cmake_minimum_required(VERSION 2.6)

project(cv-a1 C)

# add_definitions(-Wall -Werror)
set(CMAKE_EXE_LINKER_FLAGS -lobjc)

find_package(PkgConfig)
pkg_check_modules(GTK gtk+-2.0)
pkg_check_moduleS(CV opencv)

link_directories(
	${GTK_LIBRARY_DIRS}
	${CV_LIBRARY_DIRS})
include_directories(
	${GTK_INCLUDE_DIRS}
	${CV_INCLUDE_DIRS})

set(SOURCES
	main.c
	ui-builder.c
	callbacks.c
	cv-stuff.c)

set(HEADERS
	ui-builder.h
	callbacks.h
	cv-stuff.h)

add_executable(
	cv-a1 
	${SOURCES})
target_link_libraries(
	cv-a1
	${GTK_LIBRARIES}
	${CV_LIBRARIES})

