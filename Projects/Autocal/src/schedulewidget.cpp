#include "schedulewidget.h"
#include <QDateTime>

ScheduleWidget::ScheduleWidget(QWidget *parent):
	QWidget(parent), _schedule(0), _layout(new FlowLayout(5, 5, 5))
{
	setSchedule(new Schedule(this));
	setLayout(_layout);
}

Schedule *ScheduleWidget::schedule()
{
	return _schedule;
}

void ScheduleWidget::setSchedule(Schedule *schedule)
{
	if (_schedule)
		_schedule->deleteLater();

	_schedule = schedule;
	connect(_schedule, SIGNAL(tasksChanged()), this, SLOT(_tasksReload()));
	_tasksReload();
}

void ScheduleWidget::_taskRemoveRequest(Task *t)
{
	_schedule->deleteTask(_schedule->taskList().indexOf(t));
	_tasksReload();
}

void ScheduleWidget::_tasksReload()
{
	// _schedule->sortSchedule();
	TaskWidget *tw;

	while (!_widgets.isEmpty())
	{
		tw = _widgets.takeAt(0);
		tw->deleteLater();
	}

	foreach (Task *t, _schedule->taskList())
	{
		tw = new TaskWidget(128, t, this);
		connect(tw, SIGNAL(removeClicked(Task*)),
				this, SLOT(_taskRemoveRequest(Task*)));
		_widgets.append(tw);
		_layout->addWidget(tw);
	}

	if (parentWidget())
		parentWidget()->topLevelWidget()->setWindowModified(true);
}
