#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QEvent>
#include <QMainWindow>
#include "ui_mainwindow.h"
#include "schedulewidget.h"
#include "task.h"
#include "taskeditor.h"
#include "optimizedialog.h"
#include "statuswidget.h"

class MainWindow: public QMainWindow, private Ui::MainWindow
{
Q_OBJECT

public:
    MainWindow(QWidget *parent = 0);

private:
	ScheduleWidget *_scheduleWidget;
	QString _currentFilename;
	StatusWidget *_statusWidget;

private slots:
	void on_actionQuit_triggered();
	void on_actionAbout_triggered();
	void on_actionOptimize_triggered();
	void on_actionAdd_triggered();
	void on_actionSave_triggered();
	void on_actionLoad_triggered();
	void taskEditorSaveClicked(TaskEditor *editor, Task *task);
	void optimizeDialogStartClicked(OptimizeDialog *dialog,
		 uint _deadlinesWeight, uint _overlapsWeight, uint _marginsWeight);
	void closeEvent(QCloseEvent *event);
};

#endif // MAINWINDOW_H
