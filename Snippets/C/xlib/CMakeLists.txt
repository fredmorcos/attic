project (test_xlib)

find_package (X11)

if (X11_FOUND)
	include_directories (X11_INCLUDE_DIR)
	add_executable (test_xlib simple.c)
	target_link_libraries (test_xlib X11)
endif (X11_FOUND)
