#ifndef TASKEDITOR_H
#define TASKEDITOR_H

#include "ui_taskeditor.h"
#include "task.h"

class TaskEditor: public QDialog, private Ui::TaskEditor
{
Q_OBJECT

public:
    TaskEditor(QWidget *parent = 0);

private slots:
	void on_saveButton_clicked();
	void on_cancelButton_clicked();

signals:
	void saveClicked(TaskEditor *editor, Task *task);
};

#endif // TASKEDITOR_H
