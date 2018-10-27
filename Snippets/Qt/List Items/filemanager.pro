TARGET = filemanager
TEMPLATE = app
CONFIG += silent
FORMS += mainwindow.ui \
    listitemwidget.ui
SOURCES += mainwindow.cc \
    main.cc \
    listitemwidget.cc \
    listviewwidget.cc
HEADERS += mainwindow.h \
    listitemwidget.h \
    listviewwidget.h
