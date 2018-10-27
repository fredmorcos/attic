TARGET = mandelbrot
TEMPLATE = app
SOURCES += main.cc \
    mbthreaded.cc \
    mbseq.cc \
    mbgenerator.cc \
    abstractmb.cc
HEADERS += mbthreaded.h \
    mbseq.h \
    mbgenerator.h \
    abstractmb.h \
    config.h
