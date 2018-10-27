# Fred Morcos <fred.morcos@gmail.com>

subdirs(
	demo)
link_directories(
	${CAIROMM_LIBRARY_DIRS}
	${GTKMM_LIBRARY_DIRS}
	${SIGCPP_LIBRARY_DIRS})
include_directories( 
	${CAIROMM_INCLUDE_DIRS}
	${GTKMM_INCLUDE_DIRS}
	${SIGCPP_INCLUDE_DIRS})
add_library(agilith 
	engine.h 
	engine.cc
	color.h 
	color.cc 
	shape.h 
	shape.cc 
	object.h
	object.cc
	gtk.h 
	gtk.cc 
	rectangle.h
	rectangle.cc)
target_link_libraries(agilith 
	${CAIROMM_LIBRARIES}
	${SIGCPP_LIBRARIES}
	${GTKMM_LIBRARIES})

