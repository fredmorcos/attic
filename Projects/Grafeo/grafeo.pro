TEMPLATE = app
TARGET = grafeo
DEFINES += VERSION=\\\"0.0.1\\\"
QT += svg

SOURCES += main.cc

UI_DIR = build
OBJECTS_DIR = build
MOC_DIR = build
RCC_DIR = build

# QMAKE_CFLAGS += -pg -g3
# QMAKE_CXXFLAGS += -pg -g3
# QMAKE_LFLAGS += -pg -g3

include(icons/icons.pri)
include(ui/ui.pri)
include(canvas/canvas.pri)
include(widgets/widgets.pri)
include(shapes/shapes.pri)
include(extra/extra.pri)
