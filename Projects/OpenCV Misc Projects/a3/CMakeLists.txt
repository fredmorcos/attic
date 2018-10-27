cmake_minimum_required(VERSION 2.6)

project(cv3)
add_definitions(-std=c99)
set(CMAKE_EXE_LINKER_FLAGS -lobjc)

find_package(PkgConfig)
#pkg_check_modules(GTK gtk+-2.0)
pkg_check_modules(CV opencv)

set(SOURCES
	List.m
	Image.m
	Window.m
	Main.m
)

link_directories(
	#	${GTK_LIBRARY_DIRS}
	${CV_LIBRARY_DIRS}
)

include_directories(
	#	${GTK_INCLUDE_DIRS}
	${CV_INCLUDE_DIRS}
)

add_executable(cv3
	${SOURCES}
)

target_link_libraries(cv3
	#	${GTK_LIBRARIES}
	${CV_LIBRARIES}
)

#add_custom_command(
#	TARGET cv3
#	POST_BUILD
#	COMMAND gtk-builder-convert ARGS ui.glade ui.xml WORKING_DIRECTORY ${cv3_SOURCE_DIR}
#)

install(
	FILES 
		#		${cv3_SOURCE_DIR}/ui.xml
		#		${cv3_SOURCE_DIR}/color.png
				${cv3_SOURCE_DIR}/gray.png
				${cv3_SOURCE_DIR}/gray2.png
				${cv3_SOURCE_DIR}/gray3.png
				${cv3_SOURCE_DIR}/bin.bmp
				${cv3_SOURCE_DIR}/bin2.bmp
				${cv3_SOURCE_DIR}/lena-gray.png
		#		${cv3_SOURCE_DIR}/test.png
		#		${cv3_SOURCE_DIR}/black.png
		#		${cv3_SOURCE_DIR}/white.png
	DESTINATION ${cv3_BINARY_DIR}
)

