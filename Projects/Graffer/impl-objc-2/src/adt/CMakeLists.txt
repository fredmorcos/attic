link_directories(${GLIB_LIBRARY_DIRS})
include_directories(${GLIB_INCLUDE_DIRS})

add_library(adt list.m)
target_link_libraries(adt ${GLIB_LIBRARIES})

