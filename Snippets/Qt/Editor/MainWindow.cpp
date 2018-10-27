#include "MainWindow.h"
#include "MenuBar.h"

MainWindow::MainWindow(QWidget *parent)
	: QMainWindow(parent) {
	MenuBar *menuBar = new MenuBar;
	mdiArea = new MdiArea;

	connect(menuBar->documentNew, SIGNAL(triggered()), this, SLOT(documentNew()));

	this->setMenuBar(menuBar);
	this->statusBar();
	this->setMinimumSize(800, 600);
	this->setCentralWidget(mdiArea);
	this->show();
}

MainWindow::~MainWindow() {
}

void MainWindow::documentNew() {
	(mdiArea->addSubWindow(new QPlainTextEdit))->show();
}
