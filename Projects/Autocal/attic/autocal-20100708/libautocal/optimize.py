from random import uniform
from config import _debug

def start(schedule):
	elite_iterations = 0
	elite_individual = schedule
	iterations = 100

	_initial_individual(schedule)
	population = _initial_population(schedule)
	for i in range(iterations):
		_debug('iteration ' + str(i))
		population = _mutate_population(population)

		if population[0] == elite_individual:
			elite_iterations += 1
		else:
			elite_individual = population[0]
			elite_iterations = 0

		if elite_iterations == iterations / 2:
			break
	return population[0]

def _initial_individual(schedule):
	_debug('_initial_individual')
	for t in schedule.tasks:
		t.begin = t.deadline - t.duration
	_eval_individual(schedule)

def _initial_population(schedule):
	_debug('_initial_population')
	population = [schedule]
	population_size = 50
	while len(population) < population_size:
		newindiv = _mutate_individual(
			population[int(uniform(0, len(population)))])
		while newindiv in population:
			_debug('_initial_population: newindiv in population')
			newindiv = _mutate_individual(
				population[int(uniform(0, len(population)))])
		_eval_individual(newindiv)
		population += [newindiv]
	population.sort(lambda a, b: cmp(b.fitness, a.fitness))
	return population

def _mutate_population(population):
	_debug('_mutate_population')
	size = len(population)
	for i in range(size / 2):
		j = int(uniform(0, size))
		newindiv = _mutate_individual(population[j])
		while newindiv in population:
			_debug('_mutate_population: newindiv in population')
			newindiv = _mutate_individual(population[j])
		_eval_individual(newindiv)
		population += [newindiv]
	population.sort(lambda a, b: cmp(b.fitness, a.fitness))
	return population[:size]

def _mutate_individual(indiv):
	_debug('_mutate_individual')
	newindiv = indiv.__copy__()
	for t in newindiv.tasks:
		if uniform(0, 1) >= 0.5:
			if uniform(0, 1) >= 0.5:
				t.begin += max(0, int(uniform(1, t.duration / 4)))
			else:
				t.begin -= max(0, int(uniform(1, t.duration / 4)))
	return newindiv

def _eval_individual(indiv):
	_debug('_eval_individual')
	indiv.fitness = 0
	_eval_deadlines(indiv)
	_eval_overlaps(indiv)

def _eval_deadlines(indiv):
	_debug('_eval_deadlines')
	for t in indiv.tasks:
		if t.begin + t.duration > t.deadline:
			indiv.fitness -= abs(((t.begin + t.duration) - t.deadline) * 4)
		elif t.begin + t.duration == t.deadline:
			indiv.fitness += 8
		else:
			indiv.fitness -= abs((t.begin + t.duration) - t.deadline)

def _eval_overlaps(indiv):
	_debug('_eval_overlaps')
	num_overlaps = 0
	penalty = 0

	for t in indiv.tasks:
		for t2 in indiv.tasks:
			if t.begin + t.duration >= t2.begin and \
				 t.begin + t.duration <= t2.begin + t2.duration:
				penalty += ((t.begin + t.duration - t2.begin) + 1)
				num_overlaps += 1

		for t2 in indiv.fixedtasks:
			if t.begin + t.duration >= t2.begin and \
				 t.begin + t.duration <= t2.begin + t2.duration:
				penalty += ((t.begin + t.duration - t2.begin) + 1)
				num_overlaps += 1

	indiv.fitness -= (penalty * num_overlaps)
