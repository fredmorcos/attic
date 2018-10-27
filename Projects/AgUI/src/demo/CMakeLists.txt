# Fred Morcos <fred.morcos@gmail.com>

link_directories(
	.. 
	${GTKMM_LIBRARY_DIRS})
include_directories(
	.. 
	${GTKMM_INCLUDE_DIRS})
add_executable(agilith-demo
	main.cc
	canvas.h 
	canvas.cc)
target_link_libraries(agilith-demo
	agilith 
	${GTKMM_LIBRARIES})

