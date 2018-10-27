#include "task.h"

Task::Task(QObject *parent) :
	QObject(parent), _beginTime(0), _durationTime(0), _deadlineTime(0),
	_description(""), _fixed(false)
{
}

bool Task::operator<(Task &task)
{
	if(_beginTime == task.beginTime())
		return _deadlineTime > task.deadlineTime();
	return _beginTime > task.beginTime();
}

uint Task::beginTime()
{
	return _beginTime;
}

void Task::setBeginTime(uint newValue)
{
	_beginTime = newValue;
	emit beginTimeChanged(_beginTime);
	emit somethingChanged();
}

uint Task::durationTime()
{
	return _durationTime;
}

void Task::setDurationTime(uint newValue)
{
	_durationTime = newValue;
	emit durationTimeChanged(_durationTime);
	emit somethingChanged();
}

uint Task::deadlineTime()
{
	return _deadlineTime;
}

void Task::setDeadlineTime(uint newValue)
{
	_deadlineTime = newValue;
	emit deadlineTimeChanged(_deadlineTime);
	emit somethingChanged();
}

QString Task::description()
{
	return _description;
}

void Task::setDescription(QString newValue)
{
	_description = newValue;
	emit descriptionChanged(_description);
	emit somethingChanged();
}

bool Task::fixed()
{
	return _fixed;
}

void Task::setFixed(bool newValue)
{
	_fixed = newValue;
	emit fixedChanged(_fixed);
	emit somethingChanged();
}
