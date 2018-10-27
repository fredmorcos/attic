link_directories(${GTK_LIBRARY_DIRS})
include_directories(${GTK_INCLUDE_DIRS})

set(SOURCES
	object.m 
	widget.m 
	container.m 
	bin.m 
	window.m 
	button.m 
	box.m 
	vbox.m 
	hbox.m 
	drawingarea.m 
	entry.m 
	spinbutton.m 
	misc.m 
	label.m 
	separator.m 
	hseparator.m 
	vseparator.m 
	combobox.m 
	range.m 
	scale.m 
	hscale.m 
	vscale.m 
	togglebutton.m 
	progress.m 
	progressbar.m 
	notebook.m 
	checkbutton.m 
	gtk.m)

set(HEADERS
	object.h 
	widget.h 
	container.h 
	bin.h 
	window.h 
	button.h 
	box.h 
	vbox.h 
	hbox.h 
	drawingarea.h 
	entry.h 
	spinbutton.h 
	misc.h 
	label.h 
	separator.h 
	hseparator.h 
	vseparator.h 
	combobox.h 
	range.h 
	scale.h 
	hscale.h 
	vscale.h 
	togglebutton.h 
	progress.h 
	progressbar.h 
	notebook.h 
	checkbutton.h 
	objective-gtk.h)

add_library(${PROJ_NAME} SHARED ${SOURCES})
target_link_libraries(${PROJ_NAME} ${GTK_LIBRARIES})

install(TARGETS ${PROJ_NAME} 
	LIBRARY DESTINATION lib)
install(FILES ${PROJ_NAME}.pc
	DESTINATION lib/pkgconfig)
install(FILES ${HEADERS} DESTINATION include/${PROJ_NAME})

