#include "testschedule.h"

TestSchedule::TestSchedule(QObject *parent):
	QObject(parent)
{
}

void TestSchedule::init()
{
	s = new Schedule(this);
}

void TestSchedule::testOperatorEqual()
{
}

void TestSchedule::testOperatorNotEqual()
{
}

void TestSchedule::testOperatorLessThan()
{
}
