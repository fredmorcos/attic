#ifndef TASKWIDGET_H
#define TASKWIDGET_H

#include <QFrame>
#include <QCheckBox>
#include <QGraphicsOpacityEffect>
#include "task.h"
#include "ellipsislabel.h"

class TaskWidget: public QFrame
{
Q_OBJECT

public:
	TaskWidget(uint size, Task *task, QWidget *parent = 0);

private:
	Task *_task;
	EllipsisLabel *_beginLabel, *_durationLabel, *_deadlineLabel,
				  *_descriptionLabel;
	QCheckBox *_fixedCheck;
	QGraphicsOpacityEffect *_effect;

signals:
	void removeClicked(Task *t);

private slots:
	void _updateLabels();
	void _removeClicked();
	void _removeAnimationFinished();
	void _fixedCheckStateChanged(int state);
};

#endif // TASKWIDGET_H
