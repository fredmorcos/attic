TEMPLATE = app
TARGET = opengrafik
DEFINES += VERSION=\\\"0.0.1\\\"
QT += svg

SOURCES += main.cc

QMAKE_CFLAGS += -pg -g3
QMAKE_CXXFLAGS += -pg -g3
QMAKE_LFLAGS += -pg -g3

include(icons/icons.pri)
include(ui/ui.pri)
include(canvas/canvas.pri)
include(widgets/widgets.pri)
include(shapes/shapes.pri)
include(extra/extra.pri)
