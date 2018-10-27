#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include <QObject>
#include "schedule.h"
#include "evaluator.h"

class Optimizer: public QObject
{
Q_OBJECT

public:
	Optimizer(QObject *parent, uint deadlinesWeight,
			  uint overlapsWeight, uint marginsWeight);

	Schedule *start(Schedule *initial);

private:
	uint _populationSize, _iterations, _mutationRange;
	double _selectionRate, _mutationRate;
	Evaluator *_evaluator;

	void generateInitialPopulation(QList<Schedule *> &population);
	void mutatePopulation(QList<Schedule *> &population);
	Schedule *mutateIndividual(Schedule *old);
};

#endif // OPTIMIZER_H
