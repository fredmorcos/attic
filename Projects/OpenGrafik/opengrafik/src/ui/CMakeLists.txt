# Fred Morcos <fred.morcos@gmail.com>

link_directories(${GTKMM_LIBRARY_DIRS} ${GLADEMM_LIBRARY_DIRS})
include_directories(${GTKMM_INCLUDE_DIRS} ${GLADEMM_INCLUDE_DIRS})
add_library(ui ui.cc ui.h main-ui.glade)
target_link_libraries(ui ${GTKMM_LIBRARIES} ${GLADEMM_LIBRARIES})

