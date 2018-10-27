TARGET = autocal
TEMPLATE = app

# configuration variables stuff
CONFIG += \
	silent \
	warn_on \
	thread \
	qt

# enable/disable runtime benchmarks option
# qmake CONFIG+=timing
timing { 
    DEFINES += TIMING
    TIMINGON = "Yes"
}
else:TIMINGON = "No"

# enable/disable compilation debug
# qmake CONFIG+=debug
debug { 
    DEFINES += DEBUG
    DEBUGON = "Yes"
}
else {
	DEBUGON = "No"
	CONFIG += release
}

# enable/disable runtime debug outputs option
# qmake CONFIG+=noruntimedebug
noruntimedebug {
	DEFINES += QT_NO_DEBUG_OUTPUT
	RUNTIMEDEBUGON = "No"
}
else {
	RUNTIMEDEBUGON = "Yes"
}

# enable/disable runtime warn outputs option
# qmake CONFIG+=noruntimewarn
noruntimewarn {
	DEFINES += QT_NO_WARNING_OUTPUT
	RUNTIMEWARNON = "No"
}
else {
	RUNTIMEWARNON = "Yes"
}

# enable/disable unit test and some benchmarks option
# qmake CONFIG+=test
test { 
    CONFIG += qtestlib
    SOURCES += src/testmain.cpp \
        src/testoptimizer.cpp \
		src/testtask.cpp \
		src/testschedule.cpp
    HEADERS += src/testoptimizer.h \
		src/testtask.h \
		src/testschedule.h
    TESTON = "Yes"
}
else { 
    SOURCES += src/main.cpp
    TESTON = "No"
}

# sources, headers and forms
SOURCES += src/mainwindow.cpp \
    src/task.cpp \
    src/schedule.cpp \
    src/schedulewidget.cpp \
    src/taskeditor.cpp \
    src/loader.cpp \
    src/optimizedialog.cpp \
    src/optimizer.cpp \
    src/evaluator.cpp \
    src/taskwidget.cpp \
    src/flowlayout.cpp \
    src/ellipsislabel.cpp \
    src/spinner.cpp \
    src/statuswidget.cpp
HEADERS += src/mainwindow.h \
    src/task.h \
    src/schedule.h \
    src/schedulewidget.h \
    src/taskeditor.h \
    src/loader.h \
    src/optimizedialog.h \
    src/optimizer.h \
    src/evaluator.h \
    src/taskwidget.h \
    src/flowlayout.h \
    src/ellipsislabel.h \
    src/spinner.h \
	src/statuswidget.h \
	src/qiconcompat.h
FORMS += src/mainwindow.ui \
    src/taskeditor.ui \
    src/optimizedialog.ui

# enable icons for windows and mac since they suck enough to not
# provide icon themes (specs)
win32|macx {
	RESOURCES += icons/icons.qrc
}

# build dir
UI_DIR = build
OBJECTS_DIR = build
MOC_DIR = build
RCC_DIR = build

# installation prefix in case none is given
isEmpty(PREFIX) { 
    win32:PREFIX = C:/AutoCal
    unix:PREFIX = /usr/local
    macx:PREFIX = /Applications/AutoCal
}

# standard defines stuff
DEFINES += APPNAME=\\\"$$TARGET\\\"
DEFINES += VERSION=\\\"0.2\\\"
DEFINES += INSTALLPATH=\\\"$$PREFIX\\\"

# installation stuff
icon.path = $$PREFIX/share/pixmaps
icon.files = icons/128x128/autocal.png
desktop.path = $$PREFIX/share/applications
desktop.files = data/autocal.desktop
doc.path = $$PREFIX/share/doc/$$TARGET
doc.files = AUTHORS \
    ChangeLog \
    LICENSE \
    README \
    ROADMAP
target.path = $$PREFIX/bin
INSTALLS += target \
    icon \
    desktop \
    doc

# output stuff
message("Install into: $$PREFIX")
message("Unit tests: $$TESTON")
message("Debug: $$DEBUGON")
message("Timing: $$TIMINGON")
message("Runtime Debug Output: $$RUNTIMEDEBUGON")
message("Runtime Warn Output: $$RUNTIMEWARNON")
