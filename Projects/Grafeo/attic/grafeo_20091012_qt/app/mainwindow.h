#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ui_mainwindow.h"

#include <QCloseEvent>

class MainWindow : public QMainWindow, private Ui::MainWindow
{
Q_OBJECT

public:
	MainWindow(QWidget *parent = 0);

protected:
	void changeEvent(QEvent *e);
	void closeEvent(QCloseEvent *event);

private slots:
	void on_actionSave_triggered();
	void on_actionOpen_triggered();
	void saveFileSelected(const QString &filename);
};

#endif // MAINWINDOW_H
