#ifndef SCHEDULEWIDGET_H
#define SCHEDULEWIDGET_H

#include <QWidget>
#include <QList>
#include "taskwidget.h"
#include "schedule.h"
#include "flowlayout.h"

class ScheduleWidget: public QWidget
{
Q_OBJECT

public:
	ScheduleWidget(QWidget *parent = 0);

	Schedule *schedule();
	void setSchedule(Schedule *schedule);

private:
	Schedule *_schedule;
	FlowLayout *_layout;
	QList<TaskWidget *> _widgets;

private slots:
	void _tasksReload();
	void _taskRemoveRequest(Task *t);
};

#endif // SCHEDULEWIDGET_H
