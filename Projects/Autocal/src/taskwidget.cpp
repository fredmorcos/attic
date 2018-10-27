#include "taskwidget.h"
#include <QDateTime>
#include <QVBoxLayout>
#include <QToolButton>
#include <QPropertyAnimation>
#include <QSequentialAnimationGroup>

TaskWidget::TaskWidget(uint size, Task *task, QWidget *parent):
	QFrame(parent), _task(task), _beginLabel(new EllipsisLabel),
	_durationLabel(new EllipsisLabel), _deadlineLabel(new EllipsisLabel),
	_descriptionLabel(new EllipsisLabel), _fixedCheck(new QCheckBox),
	_effect(new QGraphicsOpacityEffect)
{
	setFixedSize(size * 2, size);

	_effect->setOpacity(1.0);
	setGraphicsEffect(_effect);

	connect(_task, SIGNAL(somethingChanged()), this, SLOT(_updateLabels()));
	setFrameStyle(QFrame::StyledPanel);

	_fixedCheck->setStatusTip(tr("Set task as Fixed"));
	connect(_fixedCheck, SIGNAL(stateChanged(int)),
			this, SLOT(_fixedCheckStateChanged(int)));

	QToolButton *removeButton = new QToolButton;
	removeButton->setText(tr("Remove"));
	removeButton->setIcon(QIcon::fromTheme("list-remove"));
	removeButton->setAutoRaise(true);
	removeButton->setStatusTip(tr("Remove task"));
	connect(removeButton, SIGNAL(clicked()), this, SLOT(_removeClicked()));

	QHBoxLayout *topLayout = new QHBoxLayout;
	topLayout->addWidget(_descriptionLabel);
	topLayout->addWidget(_fixedCheck);
	topLayout->addWidget(removeButton);

	QLabel *beginTitleLabel = new QLabel(tr("<b>Begin</b>"));
	QHBoxLayout *beginLayout = new QHBoxLayout;
	beginLayout->addWidget(beginTitleLabel);
	beginLayout->addStretch();
	beginLayout->addWidget(_beginLabel);

	QLabel *durationTitleLabel = new QLabel(tr("<b>Duration</b>"));
	QHBoxLayout *durationLayout = new QHBoxLayout;
	durationLayout->addWidget(durationTitleLabel);
	durationLayout->addStretch();
	durationLayout->addWidget(_durationLabel);

	QLabel *deadlineTitleLabel = new QLabel(tr("<b>Deadline</b>"));
	QHBoxLayout *deadlineLayout = new QHBoxLayout;
	deadlineLayout->addWidget(deadlineTitleLabel);
	deadlineLayout->addStretch();
	deadlineLayout->addWidget(_deadlineLabel);

	QVBoxLayout *layout = new QVBoxLayout;
	layout->addLayout(topLayout);
	layout->addLayout(beginLayout);
	layout->addLayout(durationLayout);
	layout->addLayout(deadlineLayout);

	setLayout(layout);
	_updateLabels();
}

void TaskWidget::_removeAnimationFinished()
{
	emit removeClicked(_task);
}

void TaskWidget::_removeClicked()
{
	QPropertyAnimation *opacityAnim = new QPropertyAnimation(_effect, "opacity", this),
					   *maxWidthAnim = new QPropertyAnimation(this, "maximumWidth", this),
					   *minWidthAnim = new QPropertyAnimation(this, "minimumWidth", this);
	QSequentialAnimationGroup *animGroup = new QSequentialAnimationGroup(this);

	opacityAnim->setDuration(300);
	opacityAnim->setStartValue(1.0);
	opacityAnim->setEndValue(0.0);

	minWidthAnim->setDuration(200);
	minWidthAnim->setStartValue(minimumWidth());
	minWidthAnim->setEndValue(0);

	maxWidthAnim->setDuration(200);
	maxWidthAnim->setStartValue(maximumWidth());
	maxWidthAnim->setEndValue(0);

	animGroup->addAnimation(opacityAnim);
	animGroup->addAnimation(maxWidthAnim);
	animGroup->addAnimation(minWidthAnim);

	connect(animGroup, SIGNAL(finished()), this, SLOT(_removeAnimationFinished()));

	animGroup->start(QAbstractAnimation::DeleteWhenStopped);
}

void TaskWidget::_fixedCheckStateChanged(int state)
{
	if (state == Qt::Checked)
		_task->setFixed(true);
	else
		_task->setFixed(false);
}

void TaskWidget::_updateLabels()
{
	uint time, hours, minutes;

	time = _task->durationTime();
	hours = time / 3600;
	time -= hours * 3600;
	minutes = time / 60;

	_beginLabel->setText(QString(QDateTime::fromTime_t(
		_task->beginTime()).toString("ddd MMM d hh:mm AP")));
	_deadlineLabel->setText(QString(QDateTime::fromTime_t(
		_task->deadlineTime()).toString("ddd MMM d hh:mm AP")));
	_durationLabel->setText(QString("%1 hours and %2 minutes").arg(hours).arg(minutes));
	_descriptionLabel->setText(_task->description());
	disconnect(_fixedCheck, SIGNAL(stateChanged(int)),
			   this, SLOT(_fixedCheckStateChanged(int)));
	_fixedCheck->setChecked(_task->fixed());
	connect(_fixedCheck, SIGNAL(stateChanged(int)),
			this, SLOT(_fixedCheckStateChanged(int)));
}
