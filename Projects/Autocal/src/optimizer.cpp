#include "optimizer.h"
#include <QList>
#include <QDateTime>
#include <QDebug>

#define MAX_ELITE_ITERATIONS 50
#define MAX_ITERATIONS 100
#define MAX_POPULATION_SIZE 50
#define MUTATION_NEW_INDIVIDUALS_PERCENT 0.5

Optimizer::Optimizer(QObject *parent, uint deadlinesWeight, uint overlapsWeight,
					 uint marginsWeight):
	QObject(parent), _evaluator(new Evaluator(this, deadlinesWeight,
											  overlapsWeight, marginsWeight))
{
	qsrand(QDateTime::currentDateTime().toTime_t());
}

Schedule *Optimizer::start(Schedule *initialIndividual)
{
	uint eliteIterations = 0;
	Schedule *eliteIndividual = initialIndividual;
	QList<Schedule *> population;

	_evaluator->evaluate(initialIndividual);
	population.append(initialIndividual);

	generateInitialPopulation(population);
	for (uint i = 0; i < MAX_ITERATIONS; i++)
	{
		qDebug() << "Iteration" << i;

		mutatePopulation(population);

		while (population.size() > MAX_POPULATION_SIZE)
		{
			population.last()->deleteLater();
			population.removeLast();
		}

		if (*population.at(0) == *eliteIndividual)
			++eliteIterations;
		else
		{
			eliteIndividual = population.at(0);
			eliteIterations = 0;
		}

		if (eliteIterations == MAX_ELITE_ITERATIONS)
			break;
	}

	return population.at(0);
}

void Optimizer::generateInitialPopulation(QList<Schedule *> &population)
{
	Schedule *newIndividual;

	while (population.size() < MAX_POPULATION_SIZE)
	{
		newIndividual = mutateIndividual(population.at(qrand() %
													   population.size()));

		while (population.contains(newIndividual))
			newIndividual = mutateIndividual(population.at(qrand() %
														   population.size()));

		_evaluator->evaluate(newIndividual);
		population.append(newIndividual);
	}

	qStableSort(population);
}

void Optimizer::mutatePopulation(QList<Schedule *> &population)
{
	Schedule *newIndividual;
	int j = 0;

	for (int i = 0; i < population.size() * MUTATION_NEW_INDIVIDUALS_PERCENT; i++)
	{
		j = qrand() % population.size();
		newIndividual = mutateIndividual(population.at(j));

		while (population.contains(newIndividual))
			newIndividual = mutateIndividual(population.at(j));

		_evaluator->evaluate(newIndividual);
		population.append(newIndividual);
	}

	qStableSort(population);
}

Schedule *Optimizer::mutateIndividual(Schedule *old)
{
	Schedule *newIndividual = new Schedule(old->parent());
	Task *t2;

	foreach (Task *t, old->taskList())
	{
		t2 = new Task;
		t2->setBeginTime(t->beginTime());
		t2->setDurationTime(t->durationTime());
		t2->setDeadlineTime(t->deadlineTime());
		t2->setDescription(t->description());
		t2->setFixed(t->fixed());

		newIndividual->addTask(t2);
	}

	foreach (Task *t, newIndividual->unfixedTaskList())
	{
		if (qrand() % 10 >= 5)
		{
			if (qrand() % 10 >= 5)
				t->setBeginTime(t->beginTime() + qMax(uint(0),
					(uint(qrand()) % uint(t->durationTime() / 4)) + 1));
			else
				t->setBeginTime(t->beginTime() - qMax(uint(0),
					(uint(qrand()) % uint(t->durationTime() / 4)) + 1));
		}
	}

	return newIndividual;
}
