# Fred Morcos <fred.morcos@gmail.com>

subdirs(
	canvas 
	ui)
link_directories(
	${GTKMM_LIBRARY_DIRS} 
	${GLADEMM_LIBRARY_DIRS})
include_directories(
	canvas 
	ui 
	${GTKMM_INCLUDE_DIRS} 
	${GLADEMM_INCLUDE_DIRS})
add_executable(opengrafik 
	main.cc)
target_link_libraries(opengrafik 
	canvas 
	ui 
	${GTKMM_LIBRARIES} 
	${GLADEMM_LIBRARIES})

