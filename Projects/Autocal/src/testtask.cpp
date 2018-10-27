#include "testtask.h"

TestTask::TestTask(QObject *parent):
	QObject(parent)
{
}

void TestTask::init()
{
	t1.setBeginTime(5);
	t1.setDurationTime(5);
	t1.setDeadlineTime(5);
	t1.setFixed(true);
	t1.setDescription("foo");

	t2.setBeginTime(5);
	t2.setDurationTime(5);
	t2.setDeadlineTime(5);
	t2.setFixed(true);
	t2.setDescription("foo");
}

void TestTask::testOperatorEqual()
{
	QVERIFY(t1 == t2);
}

void TestTask::testOperatorNotEqual()
{
	t2.setBeginTime(10);
	QVERIFY(t1 != t2);

	t2.setBeginTime(5);
	t2.setFixed(false);
	QVERIFY(t1 != t2);
}

void TestTask::testOperatorLessThan()
{
	t2.setBeginTime(10);
	QVERIFY(t1 < t2);
}
