#include "schedulewidget.h"
#include <QDateTime>

ScheduleWidget::ScheduleWidget(QWidget *parent):
	QTreeWidget(parent), _schedule(0)
{
	setUniformRowHeights(true);
	setAllColumnsShowFocus(true);
	setSchedule(new Schedule(this));
	setColumnCount(5);
	QStringList headers;
	headers << tr("Fixed") << tr("Start") << tr("Duration") << tr("Deadline")
			<< tr("Description");
	setHeaderLabels(headers);
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
	connect(_schedule, SIGNAL(tasksChanged()), this, SLOT(tasksReload()));
	tasksReload();
}

void ScheduleWidget::tasksReload()
{
	QList<QTreeWidgetItem *> items;
	QStringList cells;
	QString duration, begin, deadline;
	uint hours, minutes;

	_schedule->sortSchedule();

	foreach (Task *t, _schedule->taskList())
	{
		cells.clear();
		hours = 0;
		minutes = 0;

		minutes = t->durationTime() / 60;
		while (minutes >= 60)
		{
			minutes -= 60;
			hours++;
		}

		if (hours > 9)
		{
			if (minutes > 9)
				duration = "%1:%2";
			else
				duration = "%1:0%2";
		}
		else
		{
			if (minutes > 9)
				duration = "0%1:%2";
			else
				duration = "0%1:0%2";
		}

		begin = QString(QDateTime::fromTime_t(
			t->beginTime()).toString("ddd MMM d hh:mm AP"));
		deadline = QString(QDateTime::fromTime_t(
			t->deadlineTime()).toString("ddd MMM d hh:mm AP"));

		cells << (t->fixed() == true ? "Yes" : "")
			  << begin
			  << duration.arg(hours).arg(minutes)
			  << deadline
			  << t->description();
		items.append(new QTreeWidgetItem(cells));
	}

	clear();
	insertTopLevelItems(0, items);
}
