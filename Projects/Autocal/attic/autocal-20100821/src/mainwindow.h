#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QEvent>
#include <QMainWindow>
#include "schedulewidget.h"
#include "task.h"
#include "taskeditor.h"
#include "optimizedialog.h"

class MainWindow: public QMainWindow
{
Q_OBJECT

public:
    MainWindow(QWidget *parent = 0);

private:
	ScheduleWidget *_scheduleWidget;
	QString _currentFilename;

private slots:
	void on_optimizeButton_clicked();
	void on_aboutButton_clicked();
	void on_saveButton_clicked();
	void on_loadButton_clicked();
	void on_deleteButton_clicked();
	void on_addButton_clicked();
	void taskEditorSaveClicked(TaskEditor *editor, Task *task);
	void optimizeDialogStartClicked(OptimizeDialog *dialog,
		uint _deadlinesWeight, uint _overlapsWeight, uint _marginsWeight);
};

#endif // MAINWINDOW_H
