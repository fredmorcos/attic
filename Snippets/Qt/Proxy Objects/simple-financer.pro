#-------------------------------------------------
#
# Project created by QtCreator 2010-12-11T21:17:07
#
#-------------------------------------------------

QT       += core gui

TARGET = simple-financer
TEMPLATE = app


SOURCES += main.cc\
		mainwindow.cc \
    transactionmodel.cc \
    transactionitem.cc

HEADERS  += mainwindow.h \
    transactionmodel.h \
    transactionitem.h

FORMS    += mainwindow.ui
