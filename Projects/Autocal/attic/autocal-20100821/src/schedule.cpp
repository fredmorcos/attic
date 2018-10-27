#include "schedule.h"
#include <limits>

Schedule::Schedule(QObject *parent) :
	QObject(parent), _fitness(std::numeric_limits<int>::min())
{
}

void Schedule::_putUnfixedTasks()
{
	_unfixedTasks.clear();
	for (int i = 0; i < _tasks.length(); i++)
		if (!(_tasks.at(i)->fixed()))
			_unfixedTasks.append(_tasks.at(i));
}

QList<Task *> Schedule::unfixedTaskList()
{
	return _unfixedTasks;
}

QList<Task *> Schedule::taskList()
{
	return _tasks;
}

void Schedule::addTask(Task *task)
{
	_tasks.append(task);
	task->setParent(this);
	connect(task, SIGNAL(somethingChanged()), this, SLOT(someTaskChanged()));
	sortSchedule();
	if (!(task->fixed()))
		_putUnfixedTasks();
	emit tasksChanged();
}

void Schedule::deleteTask(int index)
{
	Task *t = _tasks.takeAt(index);
	t->deleteLater();
	sortSchedule();
	if (!(t->fixed()))
		_putUnfixedTasks();
	emit tasksChanged();
}

void Schedule::someTaskChanged()
{
	sortSchedule();
	_putUnfixedTasks();
	emit tasksChanged();
}

void Schedule::sortSchedule()
{
	qSort(_tasks);
}

int Schedule::fitness()
{
	return _fitness;
}

void Schedule::setFitness(int newValue)
{
	_fitness = newValue;
	emit fitnessChanged(newValue);
}
