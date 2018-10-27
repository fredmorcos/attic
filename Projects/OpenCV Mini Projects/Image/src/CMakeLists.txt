pkg_check_modules(GTK gtk+-2.0)
pkg_check_moduleS(CV opencv)

link_directories(
	${GTK_LIBRARY_DIRS} 
	${CV_LIBRARY_DIRS}
	${${PROJ_NAME}_BINARY_DIR}/lib
)

include_directories(
	${GTK_INCLUDE_DIRS} 
	${CV_INCLUDE_DIRS}
	${${PROJ_NAME}_SOURCE_DIR}/lib
)

set(SOURCES
	Main.m
	UIBuilder.m
	Callbacks.m
	CVWindow.m
	cv-stuff.m
)

set(HEADERS
	UIBuilder.h
	Callbacks.h
	CVWindow.h
	cv-stuff.h
)

add_executable(${PROJ_NAME} ${SOURCES})
target_link_libraries(${PROJ_NAME} ${GTK_LIBRARIES} ${CV_LIBRARIES})

