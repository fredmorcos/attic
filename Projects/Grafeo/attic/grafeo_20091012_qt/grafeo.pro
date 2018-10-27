TARGET = grafeo
TEMPLATE = app
QT += xml

include(app/app.pri)
include(engine/engine.pri)
include(extra/extra.pri)

!test {
	SOURCES += main.cc
}

test {
	include(test/test.pri)
}

