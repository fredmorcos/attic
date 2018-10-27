TEMPLATE = app
TARGET = grafeo
DEFINES += VERSION=\\\"0.0.1\\\" \
    APPNAME=\\\"Grafeo\\\"
CONFIG += qt
QT += svg
SOURCES += main.cc

# Some stripping out
CONFIG -= lex \
    yacc

# Debug version by default
# NOTE: anything added (+=) here should be removed
# in the release version below (-=)
DEFINES -= QT_NO_DEBUG_OUTPUT \
    QT_NO_DEBUG
CONFIG -= release \
    warn_off
CONFIG += debug \
    warn_on

# Release version
# Use: qmake CONFIG+=relver
CONFIG(relver) { 
    DEFINES += QT_NO_DEBUG_OUTPUT \
        QT_NO_DEBUG
    CONFIG += release \
        warn_off
    CONFIG -= debug \
        warn_on
}
CONFIG(test) { 
    CONFIG += qtestlib
    SOURCES -= main.cc
}
include(icons/icons.pri)
include(ui/ui.pri)
include(diagram/diagram.pri)
include(widgets/widgets.pri)
include(app/app.pri)
HEADERS += debug.h
