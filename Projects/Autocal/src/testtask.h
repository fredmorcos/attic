#ifndef TESTTASK_H
#define TESTTASK_H

#include <QObject>
#include <QTest>
#include "task.h"

class TestTask: public QObject
{
Q_OBJECT

public:
	TestTask(QObject *parent = 0);

private:
	Task t1, t2;

private slots:
	void init();
	void testOperatorEqual();
	void testOperatorNotEqual();
	void testOperatorLessThan();
};

#endif // TESTTASK_H
