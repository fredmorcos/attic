#include "taskeditor.h"

TaskEditor::TaskEditor(QWidget *parent):
	QDialog(parent)
{
    setupUi(this);
	setModal(true);
	deadlineEdit->setDateTime(QDateTime::currentDateTime());
}

void TaskEditor::on_cancelButton_clicked()
{
	hide();
	this->deleteLater();
}

void TaskEditor::on_saveButton_clicked()
{
	hide();

	Task *task = new Task();
	task->setDeadlineTime(deadlineEdit->dateTime().toTime_t());
	QDateTime dateTime;
	dateTime.setTime(durationEdit->time());
	task->setDurationTime((dateTime.time().hour() * 3600) +
						  (dateTime.time().minute() * 60));
	task->setFixed(fixedCheckBox->isChecked());
	task->setDescription(descriptionEdit->text());
	task->setBeginTime(task->deadlineTime() - task->durationTime());

	emit saveClicked(this, task);
}
