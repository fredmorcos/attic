#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"
#include <QKeyEvent>

class MainWindow: public QMainWindow, private Ui::MainWindow
{
	Q_OBJECT

public:
	MainWindow(QWidget *parent = 0);

protected:
	void keyPressEvent(QKeyEvent *event);
	void keyReleaseEvent(QKeyEvent *event);
};

#endif // MAINWINDOW_H
