#include "mainwindow.h"

MainWindow::MainWindow(QWidget *parent):
	QMainWindow(parent)
{
	setupUi(this);
	toolBar->setVisible(false);
}

void MainWindow::keyPressEvent(QKeyEvent *event)
{
	if (event->key() == Qt::Key_Control)
		toolBar->setVisible(true);
}

void MainWindow::keyReleaseEvent(QKeyEvent *event)
{
	toolBar->setVisible(false);
}
