#include "mainwindow.h"

#include <QFileDialog>

MainWindow::MainWindow(QWidget *parent):
	QMainWindow(parent)
{
	setupUi(this);
	tabWidget->removeTab(0);
}

void MainWindow::changeEvent(QEvent *e)
{
	QMainWindow::changeEvent(e);
	switch (e->type()) {
	case QEvent::LanguageChange:
		retranslateUi(this);
		break;
	default:
		break;
	}
}

void MainWindow::closeEvent(QCloseEvent *event)
{
	event->accept();
	qApp->quit();
}

void MainWindow::on_actionOpen_triggered()
{
	QFileDialog fd(this, tr("Select File to Open"));
	fd.setFileMode(QFileDialog::ExistingFile);
	fd.setOption(QFileDialog::ReadOnly);
	fd.exec();
}

void MainWindow::on_actionSave_triggered()
{
	Document *document = DocumentManager::instance()->documentFromIndex(
						 tabWidget->currentIndex());

	if (document->fileName().isEmpty() == true)
	{
		QFileDialog fd(this, tr("Select Filename to Save"));
		fd.setFileMode(QFileDialog::AnyFile);

		connect(&fd, SIGNAL(fileSelected(QString)),
				this, SLOT(saveFileSelected(QString)));

		fd.exec();
	}
	else
		DocumentManager::instance()->saveDocument(tabWidget->currentIndex(),
}

void MainWindow::saveFileSelected(const QString &filename)
{
}
