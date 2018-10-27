subdirs(adt graph algorithms ui)

set(CMAKE_EXE_LINKER_FLAGS "-lobjc -lobjective-gtk")

link_directories(
	${GTK_LIBRARY_DIRS}
	${GLIB_LIBRARY_DIRS} 
	${CAIRO_LIBRARY_DIRS} 
	adt 
	graph 
	algorithms 
	ui)
include_directories(
	${GTK_INCLUDE_DIRS} 
	${GLIB_INCLUDE_DIRS} 
	${CAIRO_INCLUDE_DIRS} 
	adt 
	graph 
	algorithms 
	ui)

add_executable(sandbox main.m)
target_link_libraries(sandbox 
	adt 
	graph 
	algorithms 
	ui 
	${GTK_LIBRARIES})

install(TARGETS sandbox
	RUNTIME DESTINATION bin
)
