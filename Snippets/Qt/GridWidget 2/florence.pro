TARGET = florence
TEMPLATE = app
SOURCES += main.cc \
    mainwindow.cc \
    manager.cc \
    loadingwidget.cc \
    gridwidget.cc \
    imagewidget.cc
HEADERS += mainwindow.h \
    manager.h \
    loadingwidget.h \
    gridwidget.h \
    imagewidget.h \
    config.h
RESOURCES += img.qrc
FORMS += loadingwidget.ui
CONFIG += silent
