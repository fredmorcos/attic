#!/usr/bin/python

from constraint import *
from order import Order
from sample import orders
from plantconfig import machines, craneMoveTime

# we create a new problem
p = Problem(BacktrackingSolver())
# p = Problem(MinConflictsSolver())

bufferEnterTimes = []
stripperEnterTimes = []
degreaserEnterTimes = []
picklerEnterTimes = []
rinserEnterTimes = []
fluxerEnterTimes = []
dryerEnterTimes = []
kettleEnterTimes = []
quencherEnterTimes = []
passivatorEnterTimes = []
postrinserEnterTimes = []

# add variables (start times for orders) as well as constraints to the problem
for i, o in enumerate(orders):
	var = str(o.id) + "-buffer"
	p.addVariable(var, range(o.startTime - craneMoveTime))
	bufferEnterTimes.append(var)

	var = str(o.id) + "-stripper"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("buffer") + 
		craneMoveTime, [var, bufferEnterTimes[i]])
	stripperEnterTimes.append(var)
    
	var = str(o.id) + "-degreaser"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("stripper") + 
		craneMoveTime, [var, stripperEnterTimes[i]])
	degreaserEnterTimes.append(var)

	var = str(o.id) + "-pickler"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("degreaser") + 
		craneMoveTime, [var, degreaserEnterTimes[i]])
	picklerEnterTimes.append(var)

	var = str(o.id) + "-rinser"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("pickler") + 
		craneMoveTime, [var, picklerEnterTimes[i]])
	rinserEnterTimes.append(var)

	var = str(o.id) + "-fluxer"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("rinser") + 
		craneMoveTime, [var, rinserEnterTimes[i]])
	fluxerEnterTimes.append(var)

	var = str(o.id) + "-dryer"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("fluxer") + 
		craneMoveTime, [var, fluxerEnterTimes[i]])
	dryerEnterTimes.append(var)

	var = str(o.id) + "-kettle"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("dryer") + 
		craneMoveTime, [var, dryerEnterTimes[i]])
	kettleEnterTimes.append(var)

	var = str(o.id) + "-quencher"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("kettle") + 
		craneMoveTime, [var, kettleEnterTimes[i]])
	quencherEnterTimes.append(var)

	var = str(o.id) + "-passivator"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("quencher") + 
		craneMoveTime, [var, quencherEnterTimes[i]])
	passivatorEnterTimes.append(var)

	var = str(o.id) + "-postrinser"
	p.addVariable(var, range(o.deadline - craneMoveTime))
	p.addConstraint(lambda x, y: x == y + o.timeAtMachine("passivator") + 
		craneMoveTime, [var, passivatorEnterTimes[i]])
	postrinserEnterTimes.append(var)

#   p.addConstraint(lambda x: x + o.timeAtMachine("postrinser") + 
#           craneMoveTime < o.deadline, [postrinserEnterTimes[i]])

for m in machines:
	m_name = m[0]

	for i, o in enumerate(orders):
		var = str(o.id) + "-" + m[0]
    
		for j, k in enumerate(orders):
			var2 = str(k.id) + "-" + m[0]
        
			if k == o:
				continue
            
			p.addConstraint(lambda x, y: x not in 
					range(y, y + orders[j].timeAtMachine(m[0]) + 2),
					[var, var2])

p.addConstraint(AllDifferentConstraint(), bufferEnterTimes)
p.addConstraint(AllDifferentConstraint(), stripperEnterTimes)
p.addConstraint(AllDifferentConstraint(), degreaserEnterTimes)
p.addConstraint(AllDifferentConstraint(), picklerEnterTimes)
p.addConstraint(AllDifferentConstraint(), rinserEnterTimes)
p.addConstraint(AllDifferentConstraint(), fluxerEnterTimes)
p.addConstraint(AllDifferentConstraint(), dryerEnterTimes)
p.addConstraint(AllDifferentConstraint(), kettleEnterTimes)
p.addConstraint(AllDifferentConstraint(), quencherEnterTimes)
p.addConstraint(AllDifferentConstraint(), passivatorEnterTimes)
p.addConstraint(AllDifferentConstraint(), postrinserEnterTimes)

# print the solutions
# for sol in p.getSolutions():
#   print sol

# print p.getSolution()

def sort_sched_bytime(a,b):
	return cmp(a[1], b[1])

def sort_sched_byorder(a,b):
	return cmp(a[0][0], b[0][0])

sols = p.getSolutions()

for s in sols:
	items = s.items()
	items.sort(sort_sched_bytime)
	items.sort(sort_sched_byorder)
	print items
print len(sols)

