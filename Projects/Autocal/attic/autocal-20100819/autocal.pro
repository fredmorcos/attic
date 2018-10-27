TARGET = autocal
TEMPLATE = app
CONFIG += silent \
    !debug \
    warn_on
SOURCES += src/main.cpp \
    src/mainwindow.cpp \
    src/task.cpp \
    src/schedule.cpp \
    src/schedulewidget.cpp \
    src/taskeditor.cpp \
    src/loader.cpp \
    src/optimizedialog.cpp \
    src/optimizer.cpp \
    src/evaluator.cpp
HEADERS += src/mainwindow.h \
    src/task.h \
    src/schedule.h \
    src/schedulewidget.h \
    src/taskeditor.h \
    src/loader.h \
    src/optimizedialog.h \
    src/optimizer.h \
    src/evaluator.h
FORMS += src/mainwindow.ui \
    src/taskeditor.ui \
    src/optimizedialog.ui
UI_DIR = build
OBJECTS_DIR = build
MOC_DIR = build
RCC_DIR = build
isEmpty(PREFIX) { 
    win32:PREFIX = C:\Program\ Files\AutoCal
    unix:PREFIX = /usr
    macx:PREFIX = /Applications/AutoCal
}
DEFINES += APPNAME=\\\"TARGET\\\"
DEFINES += VERSION=\\\"0.2\\\"
DEFINES += INSTALLPATH=\\\"$$PREFIX\\\"
icon.path = $$PREFIX/share/pixmaps
icon.files = data/autocal.png
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
