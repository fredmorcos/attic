cmake_minimum_required(VERSION 2.6)

project(cv2)
add_definitions(-std=c99)
set(CMAKE_EXE_LINKER_FLAGS "-lobjc -lpthread -lm")

find_package(PkgConfig)
pkg_check_modules(GTK gtk+-2.0)
pkg_check_modules(CV opencv)

set(SOURCES
	Extras.m
	UIBuilder.m
	Histogram.m
	Image.m
	Window.m
	Main.m
)

link_directories(
	${GTK_LIBRARY_DIRS}
	${CV_LIBRARY_DIRS}
)

include_directories(
	${GTK_INCLUDE_DIRS}
	${CV_INCLUDE_DIRS}
)

add_executable(cv2
	${SOURCES}
)

target_link_libraries(cv2
	${GTK_LIBRARIES}
	${CV_LIBRARIES}
)

add_custom_command(
	TARGET cv2
	POST_BUILD
	COMMAND gtk-builder-convert ARGS ui.glade ui.xml WORKING_DIRECTORY ${cv2_SOURCE_DIR}
)

install(
	FILES
		${cv2_SOURCE_DIR}/ui.xml
		${cv2_SOURCE_DIR}/color.png
		${cv2_SOURCE_DIR}/gray.png
		${cv2_SOURCE_DIR}/test.png
		${cv2_SOURCE_DIR}/black.png
		${cv2_SOURCE_DIR}/white.png
	DESTINATION ${cv2_BINARY_DIR}
)
