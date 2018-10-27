#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"

class MainWindow : public QMainWindow, private Ui::MainWindow {
	Q_OBJECT
public:
	MainWindow(QWidget *parent = 0);

private slots:
	void on_addButton_clicked();
};

#endif // MAINWINDOW_H
