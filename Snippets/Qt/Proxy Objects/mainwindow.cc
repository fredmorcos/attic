#include "mainwindow.h"
#include "transactionmodel.h"
#include <QSortFilterProxyModel>

MainWindow::MainWindow(QWidget *parent):
	QMainWindow(parent)
{
	setupUi(this);

	TransactionModel *model = new TransactionModel();
	model->addItem(new TransactionItem(0, 1.0, QDate::currentDate(), "Shopping", false));
	model->addItem(new TransactionItem(0, 2.0, QDate::currentDate().addDays(-1), "Car", false));
	model->addItem(new TransactionItem(0, 3.0, QDate::currentDate().addDays(-2), "Salary", true));
	QSortFilterProxyModel *proxyModel = new QSortFilterProxyModel();
	proxyModel->setSourceModel(model);
	treeView->setModel(proxyModel);
}
