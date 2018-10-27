link_directories(${GTK_LIBRARY_DIRS})
include_directories(${GTK_INCLUDE_DIRS})
add_library(ui 
	general.m 
	app.m 
	file.m 
	layout.m)

target_link_libraries(ui 
	${GTK_LIBRARIES}
)

