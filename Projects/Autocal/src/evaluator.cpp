#include "evaluator.h"

Evaluator::Evaluator(QObject *parent, uint deadlinesWeight,
					 uint overlapsWeight, uint marginsWeight):
	QObject(parent), _deadlinesWeight(deadlinesWeight),
	_overlapsWeight(overlapsWeight), _marginsWeight(marginsWeight)
{
}

void Evaluator::evaluate(Schedule *schedule)
{
	int result = 0;

	if (_deadlinesWeight)
		result += _overlapsWeight * _evaluateDeadlinesHelper(schedule);
	if (_overlapsWeight)
		result += _overlapsWeight * _evaluateOverlapsHelper(schedule);
	if (_marginsWeight)
		result += _marginsWeight * _evaluateMarginsHelper(schedule);

	schedule->setFitness(result);
}

int Evaluator::_evaluateDeadlinesHelper(Schedule *schedule)
{
	int result = 0;

	foreach (Task *t, schedule->taskList())
		result += ((int)(t->deadlineTime())) -
				  ((int)(t->beginTime() + t->durationTime()));

	return result;
}

int Evaluator::_evaluateOverlapsHelper(Schedule *schedule)
{
	int result = 0;
	uint t_endTime = 0, t2_endTime = 0;

	foreach (Task *t, schedule->taskList())
	{
		foreach (Task *t2, schedule->taskList())
		{
			if (t != t2)
			{
				t_endTime = t->beginTime() + t->durationTime();
				t2_endTime = t2->beginTime() + t2->durationTime();

				if (t2->beginTime() >= t->beginTime() &&
					 t2->beginTime() <= t_endTime)
				{
					result -= (t_endTime - t2->beginTime());
				}
				/*
				else if ((t2_endTime >= t->beginTime() &&
						  t2_endTime <= t_endTime))
				{
				}
				*/
			}
		}
	}

	return result;
}

int Evaluator::_evaluateMarginsHelper(Schedule *schedule)
{
	int result = 0, tmp = 0;

	foreach (Task *t, schedule->taskList())
	{
		foreach (Task *t2, schedule->taskList())
		{
			if (t != t2)
			{
				tmp = qAbs((int)(t->beginTime()) + (int)(t->durationTime()) -
						  (int)(t2->beginTime()));
				if (tmp < 2 * 60 * 60)
				{
					result -= tmp;
				}
			}
		}
	}

	return result;
}
