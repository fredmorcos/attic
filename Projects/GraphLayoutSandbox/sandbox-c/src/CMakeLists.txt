link_directories(${GTK_LIBRARY_DIRS})
include_directories(${GTK_INCLUDE_DIRS})
add_executable(sandbox point.c vertex.c edge.c graph.c table.c random.c main.c)
target_link_libraries(sandbox ${GTK_LIBRARIES})
