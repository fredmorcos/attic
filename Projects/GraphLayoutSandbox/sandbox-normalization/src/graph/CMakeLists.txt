link_directories(${CAIRO_LIBRARY_DIRS})
include_directories(${CAIRO_INCLUDE_DIRS})

add_library(graph point.m edge.m node.m gravity.m graph.m)
target_link_libraries(graph adt ${CAIRO_LIBRARIES})

