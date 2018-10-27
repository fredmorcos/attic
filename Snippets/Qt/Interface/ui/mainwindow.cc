#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "addstoragedialog.h"
#include "additemdialog.h"

MainWindow::MainWindow(QWidget *parent)
	: QMainWindow(parent), ui(new Ui::MainWindow)
{
	ui->setupUi(this);
}

MainWindow::~MainWindow()
{
	delete ui;
}

void MainWindow::on_actionNewStorage_triggered()
{
	new AddStorageDialog(this);
}

void MainWindow::on_actionNew_Item_triggered()
{
	new AddItemDialog(this);
}
