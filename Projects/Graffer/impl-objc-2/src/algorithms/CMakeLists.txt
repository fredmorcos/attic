link_directories(graph)
include_directories(graph)

add_library(algorithms layout-fr.m layout-simple.m layout.m nodes.m)
target_link_libraries(algorithms graph)

