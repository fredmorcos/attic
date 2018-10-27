#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtGui/QtGui>
#include "MdiArea.h"

class MainWindow : public QMainWindow {
Q_OBJECT

public:
	MainWindow(QWidget *parent = 0);
	~MainWindow();

private:
	MdiArea *mdiArea;

private slots:
	void documentNew();
};

#endif /* MAINWINDOW_H */
