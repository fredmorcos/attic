import re
from config import _error
from schedule import Schedule, Task

def save(sched):
	res = ''
	task = '{\n\tdescription = %s\n\tdeadline = %s\n\tbegin = %s\n\tduration = %s\n\tfixed = %s\n}\n'
	for t in sched.tasks:
		res += (task % (str(t.id), t.description, str(t.deadline), str(t.begin), str(t.duration), '0'))
	for t in sched.fixedtasks:
		res += (task % (str(t.id), t.description, str(t.deadline), str(t.begin), str(t.duration), '1'))
	return res

def load(data):
	id = None
	description = None
	deadline = None
	begin = None
	duration = None
	fixed = None

	_sched = Schedule()

	for _lineno, _line in enumerate(data.split('\n')):
		line = _line.strip()
		if not line.startswith('#') and line != '':
			regexp_res = re.match(r'(?P<task_id>[0-9]+)[ \t]*{', line)
			if regexp_res:
				id = int(regexp_res.group('task_id'))
				continue

			regexp_res = re.match(
				r'(?P<prop_name>[a-zA-Z]+)[ \t]*=[ \t]*(?P<prop_val>.*)', line)
			if regexp_res:
				if regexp_res.group('prop_name') == 'description':
					description = str(regexp_res.group('prop_val'))
				elif regexp_res.group('prop_name') == 'deadline':
					deadline = int(regexp_res.group('prop_val'))
				elif regexp_res.group('prop_name') == 'begin':
					begin = int(regexp_res.group('prop_val'))
				elif regexp_res.group('prop_name') == 'duration':
					duration = int(regexp_res.group('prop_val'))
				elif regexp_res.group('prop_name') == 'fixed':
					fixed = int(regexp_res.group('prop_val'))
				continue

			if line == '}':
				t = Task(id, begin, duration, deadline, description, fixed)
				if fixed == 0:
					_sched.tasks += [t]
				else:
					_sched.fixedtasks += [t]
				continue

			_error('Error at line ' + str(_lineno))
	return _sched
