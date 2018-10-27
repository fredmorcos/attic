#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"

class Graph;

class MainWindow : public QMainWindow, private Ui::MainWindow
{
Q_OBJECT

private:
	Graph *scene;

public:
    MainWindow(QWidget *parent = 0);

protected:
    void changeEvent(QEvent *e);

private slots:
	void on_layoutButton_toggled(bool checked);
 void on_edgesButton_toggled(bool checked);
	void on_nodesButton_toggled(bool checked);
	void on_selectButton_toggled(bool checked);
};

#endif // MAINWINDOW_H
