# Fred Morcos <fred.morcos@gmail.com>

subdirs(
	demo)
link_directories(
	${CAIROMM_LIBRARY_DIRS}
	${GTKMM_LIBRARY_DIRS})
include_directories(
	${CAIROMM_INCLUDE_DIRS}
	${GTKMM_INCLUDE_DIRS})
add_library(canvas 
	canvas.h 
	canvas.cc 
	color.h 
	color.cc 
	shape.h 
	shape.cc)
target_link_libraries(canvas 
	${CAIROMM_LIBRARIES})

add_library(gtkcanvas 
	gtk-canvas.h
	gtk-canvas.cc)
target_link_libraries(gtkcanvas 
	${GTKMM_LIBRARIES}
	canvas)

