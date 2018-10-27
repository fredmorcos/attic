include_directories (/usr/include/gtk-2.0/ /usr/include/cairo/)
add_executable (bling main.c bling.c prefs.c)
target_link_libraries (bling /usr/lib/libgtk-x11-2.0.so /usr/lib/libcairo.so)
