#include "mainwindow.h"
#include "listviewwidget.h"

MainWindow::MainWindow(QWidget *parent):
		QMainWindow(parent)
{
	setupUi(this);
	setWindowTitle("Ereis File Manager");

	ListViewWidget *w = new ListViewWidget(this);
	mainLayout->addWidget(w);
}
