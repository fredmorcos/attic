#ifndef SCHEDULEWIDGET_H
#define SCHEDULEWIDGET_H

#include <QTreeWidget>
#include "schedule.h"

class ScheduleWidget : public QTreeWidget
{
Q_OBJECT

public:
	ScheduleWidget(QWidget *parent = 0);

	Schedule *schedule();
	void setSchedule(Schedule *schedule);

private:
	Schedule *_schedule;

private slots:
	void tasksReload();
};

#endif // SCHEDULEWIDGET_H
