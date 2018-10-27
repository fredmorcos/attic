#include "mainwindow.h"
#include "loader.h"
#include "optimizer.h"
#include "qiconcompat.h"
#include <QFileDialog>
#include <QMessageBox>

#include <QDebug>

#define ICON QString(INSTALLPATH) + QString("/share/pixmaps/autocal.png")

MainWindow::MainWindow(QWidget *parent):
	QMainWindow(parent), _scheduleWidget(new ScheduleWidget(parent)),
	_currentFilename(""), _statusWidget(new StatusWidget(this))
{
    setupUi(this);

	actionLoad->setIcon(QICON_FROM_THEME("document-open"));
	actionSave->setIcon(QICON_FROM_THEME("document-save"));
	actionOptimize->setIcon(QICON_FROM_THEME("system-run"));
	actionQuit->setIcon(QICON_FROM_THEME("application-exit"));
	actionAbout->setIcon(QICON_FROM_THEME("help-about"));
	actionAdd->setIcon(QICON_FROM_THEME("list-add"));

	if (QFile(ICON).exists())
		setWindowIcon(QIcon(ICON));
	else if (QFile("icons/128x128/autocal.png").exists())
		setWindowIcon(QIcon("icons/128x128/autocal.png"));
	else if (QFile("../icons/128x128/autocal.png").exists())
		setWindowIcon(QIcon("../icons/128x128/autocal.png"));
	else
		setWindowIcon(QIcon(":/icons/128x128/autocal.png"));

	scrollArea->setWidget(_scheduleWidget);
	QMainWindow::statusBar()->addWidget(_statusWidget);
}

void MainWindow::closeEvent(QCloseEvent *event)
{
	on_actionQuit_triggered();
	QMainWindow::closeEvent(event);
}

void MainWindow::taskEditorSaveClicked(TaskEditor *editor, Task *task)
{
	_scheduleWidget->schedule()->addTask(task);
	editor->deleteLater();
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

void MainWindow::on_actionLoad_triggered()
{
	QString errorMessage;
	QString tmp = QFileDialog::getOpenFileName(this,
		tr("Load Calendar"), "", "AutoCal Calendar (*.acf);;All Files (*.*)",
		new QString("AutoCal Calendar(*.acf)"), QFileDialog::ReadOnly);
	if (tmp == "")
		return;

	_statusWidget->showMessage(tr("Loading file..."), true, 0);

	QString filename = tmp;
	Loader *loader = new Loader(this);
	Schedule *newSchedule = loader->loadFromFile(filename);
	// newSchedule->sortSchedule();
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

	QFileInfo fi(filename);
	setWindowTitle(fi.baseName() + "[*] - " +
				   tr("Automatic Calendar and Event Management"));
	setWindowModified(false);
	_statusWidget->showMessage(tr("Loaded."), false, 5000);
}

void MainWindow::on_actionSave_triggered()
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

	_statusWidget->showMessage(tr("Saving file..."), true, 0);

	if (!filename.endsWith(".acf"))
		filename += ".acf";

	Loader *loader = new Loader(this);
	loader->saveToFile(_scheduleWidget->schedule(), filename);
	_currentFilename = filename;

	QFileInfo fi(filename);
	setWindowTitle(fi.baseName() + "[*] - " +
				   tr("Automatic Calendar and Event Management"));
	setWindowModified(false);
	_statusWidget->showMessage(tr("Saved."), false, 5000);
}

void MainWindow::on_actionAdd_triggered()
{
	TaskEditor *editor = new TaskEditor(this);
	connect(editor, SIGNAL(saveClicked(TaskEditor*,Task*)),
			this, SLOT(taskEditorSaveClicked(TaskEditor*,Task*)));
	editor->show();
}

void MainWindow::on_actionOptimize_triggered()
{
	OptimizeDialog *dialog = new OptimizeDialog(this);
	connect(dialog, SIGNAL(startClicked(OptimizeDialog*,uint,uint,uint)), this,
			SLOT(optimizeDialogStartClicked(OptimizeDialog*,uint,uint,uint)));
	dialog->show();
}

void MainWindow::on_actionAbout_triggered()
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

void MainWindow::on_actionQuit_triggered()
{
	if (isWindowModified())
	{
		QMessageBox::StandardButton sb =
			QMessageBox::question(this, tr("Unsaved Changes"),
								  tr("The schedule has been modified. Save?"),
								  QMessageBox::Save | QMessageBox::Discard);

		if (sb == QMessageBox::Save)
			on_actionSave_triggered();
	}

	qApp->quit();
}
