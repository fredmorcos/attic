#include "testoptimizer.h"
#include "task.h"
#include <QDebug>

TestOptimizer::TestOptimizer(QObject *parent):
	QObject(parent)
{
}

void TestOptimizer::init()
{
	o = new Optimizer(this, 5, 5, 5);
}

void TestOptimizer::testT1()
{
	s = new Schedule(this);
	Task *t;

	for (uint i = 1; i <= 10; i++)
	{
		t = new Task(s);
		t->setDeadlineTime(i * 10);
		t->setDurationTime(5);
		t->setDescription(QString("Task %1").arg(i));
		t->setBeginTime(t->deadlineTime() - t->durationTime());
		t->setFixed(false);

		// QWARN(QString("%1 Begin Time: %2").arg(t->description()).arg(t->beginTime()).toStdString().c_str());

		s->addTask(t);
	}

	qDebug() << *s;

	QBENCHMARK
	{
		ns = o->start(s);
	}

	qDebug() << *ns;
}
