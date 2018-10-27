subdirs(adt graph algorithms ui)

set(CMAKE_EXE_LINKER_FLAGS -lobjc)

link_directories(
	${GTK_LIBRARY_DIRS} 
	${CAIRO_LIBRARY_DIRS} 
	adt 
	graph 
	algorithms 
	ui
)

include_directories(
	${GTK_INCLUDE_DIRS} 
	${CAIRO_INCLUDE_DIRS} 
	adt 
	graph 
	algorithms 
	ui
)

add_executable(grafer main.m)
target_link_libraries(grafer 
	adt 
	graph 
	algorithms 
	ui 
	${OBGTK_LIBRARIES} 
)

install(TARGETS grafer
	RUNTIME DESTINATION bin
)

