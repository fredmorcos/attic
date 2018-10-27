#ifndef TESTOPTIMIZER_H
#define TESTOPTIMIZER_H

#include <QObject>
#include <QTest>
#include "optimizer.h"
#include "schedule.h"

class TestOptimizer: public QObject
{
Q_OBJECT

public:
	TestOptimizer(QObject *parent = 0);

private:
	Optimizer *o;
	Schedule *s, *ns;

private slots:
	void init();
	void testT1();
};

#endif // TESTOPTIMIZER_H
