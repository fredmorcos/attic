#ifndef SCHEDULE_H
#define SCHEDULE_H

#include <QObject>
#include <QList>
#include "task.h"

class Schedule: public QObject
{
Q_OBJECT

Q_PROPERTY(int fitness READ fitness WRITE setFitness
		   NOTIFY fitnessChanged)

public:
	Schedule(QObject *parent = 0);

	void addTask(Task *task);
	void deleteTask(int index);

	int fitness();
	void setFitness(int newValue);

	void sortSchedule();

	QList<Task *> taskList();
	QList<Task *> unfixedTaskList();

private:
	QList<Task *> _tasks, _unfixedTasks;
	int _fitness;

	void _putUnfixedTasks();

private slots:
	void someTaskChanged();

signals:
	void fitnessChanged(int newValue);
	void tasksChanged();
};

#endif // SCHEDULE_H
