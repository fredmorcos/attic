#ifndef EVALUATOR_H
#define EVALUATOR_H

#include <QObject>
#include "schedule.h"

class Evaluator: public QObject
{
Q_OBJECT

public:
	Evaluator(QObject *parent = 0, uint deadlinesWeight = 0,
			  uint overlapsWeight = 0, uint marginsWeight = 0);

	void evaluate(Schedule *schedule);

private:
	uint _deadlinesWeight, _overlapsWeight, _marginsWeight;

	int _evaluateDeadlinesHelper(Schedule *schedule);
	int _evaluateOverlapsHelper(Schedule *schedule);
	int _evaluateMarginsHelper(Schedule *schedule);
};

#endif // EVALUATOR_H
