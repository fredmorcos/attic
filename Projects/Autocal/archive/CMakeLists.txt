cmake_minimum_required(VERSION 2.8)

project(autocal)
find_package(Qt4 REQUIRED)
add_definitions(-DUSING_CMAKE)
add_definitions("-Wall")
set(autocal_VERSION "0.2")

set(autocal_test_sources )
set(autocal_test_headers )

# build options
option(PROFILING_BENCHMARKS "Set to ON for the application to produce profiling and benchmarking console output and graphs" OFF)
if (PROFILING_BENCHMARKS)
	add_definitions(-DPERF_STUFF)
endif()

option(TESTING "Set to ON to build the application in test and benchmark mode" OFF)
if (TESTING)
	add_definitions(-DTEST_STUFF)
	set(QT_USE_QTTEST TRUE)

	set(autocal_test_sources
		src/testtask.cpp
	)

	set(autocal_test_headers
		src/testtask.h
	)
endif()

# set default cmake build type to RelWithDebInfo (None Debug Release RelWithDebInfo MinSizeRel)
if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE "MinSizeRel" )
endif()

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
	src/taskwidget.cpp
	src/flowlayout.cpp
	src/ellipsislabel.cpp
	src/spinner.cpp
	src/statuswidget.cpp

	${autocal_test_sources}
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
	src/taskwidget.h
	src/flowlayout.h
	src/ellipsislabel.h
	src/spinner.h
	src/statuswidget.h

	${autocal_test_headers}
)

set(autocal_forms
	src/mainwindow.ui
	src/optimizedialog.ui
	src/taskeditor.ui
)

# begin: icon resources for windows and mac os x
set(autocal_resources )

if(!UNIX)
	set(autocal_resources
		icons/icons.qrc
	)
endif()

qt4_wrap_cpp(autocal_headers_moc ${autocal_headers})
qt4_wrap_ui(autocal_forms_headers ${autocal_forms})
qt4_add_resources(autocal_resources_rcc ${autocal_resources})

include(${QT_USE_FILE})
add_definitions(${QT_DEFINITIONS})

add_executable(autocal
			   ${autocal_sources}
			   ${autocal_headers_moc}
			   ${autocal_forms_headers}
			   ${autocal_resources_rcc}
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

