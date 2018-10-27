#include "mainwindow.h"
#include "pane.h"
#include <QPushButton>

MainWindow::MainWindow(QWidget *parent) :
	QMainWindow(parent){
	setupUi(this);
}

void MainWindow::on_addButton_clicked()
{
	Pane *p = new Pane();
	panesLayout->addWidget(p);
	p->show();
}
