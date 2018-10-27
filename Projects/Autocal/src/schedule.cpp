#include "schedule.h"
#include <QtGlobal>
#include <limits>

Schedule::Schedule(QObject *parent) :
	QObject(parent), _fitness(std::numeric_limits<int>::min())
{
}

bool Schedule::operator<(Schedule &schedule)
{
	return _fitness > schedule.fitness();
}

bool Schedule::operator==(Schedule &schedule)
{
	foreach (Task *t, _tasks)
		foreach (Task *t2, schedule.taskList())
			if (*t != *t2)
				return false;
	return true;
}

QDebug Schedule::operator<<(Schedule &s)
{
	QDebug dbg(QtDebugMsg);

	dbg << "Schedule";
	dbg << "\t No. Fixed Tasks:" << s.taskList().size() - s.unfixedTaskList().size();
	dbg << "\t No. Unfixed Tasks:" << s.unfixedTaskList().size();
	dbg << "\t No. Tasks (Total):" << s.taskList().size();
	dbg << "------------";
	dbg << "Tasks";
	foreach (Task *t, s.taskList())
	{
		dbg << "\tTask";
		dbg << "\t\tDescription:" << t->description();
		dbg << "\t\tBegin Time:" << t->beginTime();
		dbg << "\t\tDuration:" << t->durationTime();
		dbg << "\t\tDeadline:" << t->deadlineTime();
		dbg << "\t\tFixed:" << t->fixed();
		dbg << "\t\t------------";
	}
	dbg << "------------";

	return dbg;
}

void Schedule::_putUnfixedTasks()
{
	_unfixedTasks.clear();
	foreach (Task *t, _tasks)
		if (!(t->fixed()))
			_unfixedTasks.append(t);
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
	_putUnfixedTasks();
	emit tasksChanged();
}

void Schedule::deleteTask(uint index)
{
	Task *t = _tasks.takeAt(index);
	t->deleteLater();
	sortSchedule();
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
	if (_tasks.isEmpty())
		return;

	qStableSort(_tasks);
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
