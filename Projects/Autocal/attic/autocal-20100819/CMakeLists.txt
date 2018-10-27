cmake_minimum_required(VERSION 2.8)
project(autocal)
find_package(Qt4 REQUIRED)

set(autocal_VERSION "0.2")
configure_file(
	"${PROJECT_SOURCE_DIR}/src/config.h.in"
	"${PROJECT_BINARY_DIR}/src/config.h"
)

set(autocal_sources
	src/evaluator.cpp
	src/loader.cpp
	src/main.cpp
	src/mainwindow.cpp
	src/optimizedialog.cpp
	src/optimizer.cpp
	src/schedule.cpp
	src/schedulewidget.cpp
	src/task.cpp
	src/taskeditor.cpp
)

set(autocal_headers
	src/evaluator.h
	src/loader.h
	src/mainwindow.h
	src/optimizedialog.h
	src/optimizer.h
	src/schedule.h
	src/schedulewidget.h
	src/taskeditor.h
	src/task.h
)

set(autocal_forms
	src/mainwindow.ui
	src/optimizedialog.ui
	src/taskeditor.ui
)

# set(autocal_resources
#	stuff.qrc
# )

qt4_wrap_cpp(autocal_headers_moc ${autocal_headers})
qt4_wrap_ui(autocal_forms_headers ${autocal_forms})
# qt4_wrap_resoures(autocal_resources_rcc ${autocal_resources})

include(${QT_USE_FILE})
add_definitions(${QT_DEFINITIONS})

add_executable(autocal
			   ${autocal_sources}
			   ${autocal_headers_moc}
			   ${autocal_forms_headers}
)

target_link_libraries(autocal ${QT_LIBRARIES})
include_directories(${CMAKE_CURRENT_BINARY_DIR} "${PROJECT_BINARY_DIR}/src")

install(TARGETS autocal DESTINATION bin)
install(FILES "${PROJECT_SOURCE_DIR}/data/autocal.png" DESTINATION share/pixmaps)
install(FILES "${PROJECT_SOURCE_DIR}/data/autocal.desktop" DESTINATION share/applications)
install(FILES
		"${PROJECT_SOURCE_DIR}/AUTHORS"
		"${PROJECT_SOURCE_DIR}/ChangeLog"
		"${PROJECT_SOURCE_DIR}/LICENSE"
		"${PROJECT_SOURCE_DIR}/README"
		"${PROJECT_SOURCE_DIR}/ROADMAP"
		DESTINATION
		share/doc/autocal
)

