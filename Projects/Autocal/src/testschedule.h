#ifndef TESTSCHEDULE_H
#define TESTSCHEDULE_H

#include <QObject>
#include <QTest>
#include "schedule.h"

class TestSchedule: public QObject
{
Q_OBJECT

public:
	TestSchedule(QObject *parent = 0);

private:
	Schedule *s;

private slots:
	void init();
	void testOperatorEqual();
	void testOperatorNotEqual();
	void testOperatorLessThan();
};

#endif // TESTSCHEDULE_H
