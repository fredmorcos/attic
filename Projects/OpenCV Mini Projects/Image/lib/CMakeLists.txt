pkg_check_modules(GTK gtk+-2.0)
pkg_check_moduleS(CV opencv)

link_directories(
	${GTK_LIBRARY_DIRS} 
	${CV_LIBRARY_DIRS}
)

include_directories(
	${GTK_INCLUDE_DIRS} 
	${CV_INCLUDE_DIRS}
)

set(SOURCES
	Image.m
)

set(HEADERS
	Point.h
	Image.h
)

add_library(
	${LIB_NAME} 
	SHARED
	${SOURCES}
)

target_link_libraries(
	${LIB_NAME} 
	${GTK_LIBRARIES} 
	${CV_LIBRARIES}
)

install(TARGETS ${LIB_NAME} LIBRARY DESTINATION lib)
install(FILES ${LIB_NAME}.pc DESTINATION lib/pkgconfig)
install(FILES ${HEADERS} DESTINATION include/${LIB_NAME})

