#include "mainwindow.h"
#include "button.h"

MainWindow::MainWindow(QWidget *parent)
	: QMainWindow(parent),
	widget(new QWidget(this)),
	button(new QPushButton("Add", widget)),
	m_indexes(1)
{
	setCentralWidget(widget);

	connect(button, SIGNAL(clicked()), this, SLOT(add_clicked()));
}

void MainWindow::add_clicked()
{
	Button *x = new Button(m_indexes++, widget);
	x->show();
}
