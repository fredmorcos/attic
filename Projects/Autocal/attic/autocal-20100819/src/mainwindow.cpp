#include "mainwindow.h"
#include "loader.h"
#include "optimizer.h"
#include "config.h"
#include <QFileDialog>
#include <QMessageBox>

#define ICON QString(INSTALLPATH) + QString("/share/pixmaps/autocal.png")

MainWindow::MainWindow(QWidget *parent):
	QMainWindow(parent), _scheduleWidget(new ScheduleWidget(parent)),
	_currentFilename("")
{
    setupUi(this);
	horizontalLayout->addWidget(_scheduleWidget, 1);

	if (QFile(ICON).exists())
		setWindowIcon(QIcon(ICON));
	else
		setWindowIcon(QIcon("data/autocal.png"));
}

void MainWindow::on_addButton_clicked()
{
	TaskEditor *editor = new TaskEditor(this);
	connect(editor, SIGNAL(saveClicked(TaskEditor*,Task*)),
			this, SLOT(taskEditorSaveClicked(TaskEditor*,Task*)));
	editor->show();
}

void MainWindow::taskEditorSaveClicked(TaskEditor *editor, Task *task)
{
	_scheduleWidget->schedule()->addTask(task);
	editor->deleteLater();
}

void MainWindow::on_deleteButton_clicked()
{
	if (_scheduleWidget->currentItem())
		_scheduleWidget->schedule()->deleteTask(
			_scheduleWidget->indexOfTopLevelItem(
			_scheduleWidget->currentItem()));
}

void MainWindow::on_loadButton_clicked()
{
	QString errorMessage;
	QString tmp = QFileDialog::getOpenFileName(this,
		tr("Load Calendar"), "", "AutoCal Calendar (*.acf);;All Files (*.*)",
		new QString("AutoCal Calendar(*.acf)"), QFileDialog::ReadOnly);
	if (tmp == "")
		return;
	QString filename = tmp;
	Loader *loader = new Loader(this);
	Schedule *newSchedule = loader->loadFromFile(filename);
	if (newSchedule)
	{
		_scheduleWidget->setSchedule(newSchedule);
		_currentFilename = filename;
	}
	else
	{
		if (loader->lineNumber() == -1)
			errorMessage = tr("File not found.");
		else
			errorMessage = tr("Error at line %1.").arg(loader->lineNumber());
		QMessageBox::critical(this, tr("Error"), errorMessage,
			QMessageBox::Ok);
	}
	loader->deleteLater();
}

void MainWindow::on_saveButton_clicked()
{
	QString filename = "", tmp;

	if (_currentFilename.isEmpty())
	{
		tmp = QFileDialog::getSaveFileName(this, tr("Save Calendar"), "",
			"AutoCal Calendar (*.acf);;All Files (*.*)",
			new QString("AutoCal Calendar(*.acf)"));
		if (tmp == "")
			return;
		filename = tmp;
	}
	else
		filename = _currentFilename;

	Loader *loader = new Loader(this);
	loader->saveToFile(_scheduleWidget->schedule(), filename);
	_currentFilename = filename;
}

void MainWindow::on_aboutButton_clicked()
{
	QMessageBox::about(this, tr("About Autocal"),
		QString("<p><center><big><b>AutoCal %1</b></big><br/>").arg(VERSION)
		// + "<img src=\"" + ICON + "\"/><br/>"
		+ tr("<small>Automatic Calendar Management</small><br/><br/>")
		+ tr("Copyright (c) 2010 Fred Morcos<br/>")
		+ tr("<small>Released under the GPLv3 license</small><br/><br/>")
		+ "<a href=\"http://autocal.googlecode.com/\">"
		+ "http://autocal.googlecode.com/</a><br/>"
		+ "<a href=\"http://autocal.gitorious.org/\">"
		+ "http://autocal.gitorious.org/</a></center></p>"
	);
}

void MainWindow::on_optimizeButton_clicked()
{
	OptimizeDialog *dialog = new OptimizeDialog(this);
	connect(dialog, SIGNAL(startClicked(OptimizeDialog*,uint,uint,uint)), this,
			SLOT(optimizeDialogStartClicked(OptimizeDialog*,uint,uint,uint)));
	dialog->show();
}

void MainWindow::optimizeDialogStartClicked(OptimizeDialog *dialog,
	uint deadlinesWeight, uint overlapsWeight, uint marginsWeight)
{
	dialog->deleteLater();
	Optimizer *optimizer = new Optimizer(this, deadlinesWeight, overlapsWeight,
		marginsWeight);
	_scheduleWidget->setSchedule(optimizer->start(_scheduleWidget->schedule()));
	optimizer->deleteLater();
}
