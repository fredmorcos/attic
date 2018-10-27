#include "optimizer.h"

Optimizer::Optimizer(QObject *parent, uint deadlinesWeight, uint overlapsWeight,
					 uint marginsWeight):
	QObject(parent), _evaluator(new Evaluator(this, deadlinesWeight,
											  overlapsWeight, marginsWeight))
{
}

Schedule *Optimizer::start(Schedule *initial)
{
}
