.data
zero:	0,
one:	1,
two:	2,
	
a:	3,
b:	7,

p:	0,
q:	0,
n:	0,
r:	0,

i:	1,
d:	2,

tmp:	0,
dx2:	0

.text

mul:
	b,	one,	tmp,	mul_end
	b,	zero,	n,	foo
	zero,	zero,	q,	foo

half:
	n,	two,	tmp,	half_end
	two,	zero,	d,	foo
	one,	zero,	i,	foo

inhalf:
	zero,	d,	tmp,	_p1
_p1:
	d,	tmp,	dx2,	foo
	n,	dx2,	tmp,	foo
	tmp,	zero,	tmp,	inhalf_end
	d,	zero,	i,	foo
	dx2,	zero,	d,	foo
	zero,	one,	tmp,	inhalf

inhalf_end:
	zero,	i,	tmp,	_p2

_p2:
	q,	tmp,	q,	foo
	n,	d,	n,	foo
	zero,	one,	tmp,	half

half_end:
	q,	zero,	b,	foo
	n,	zero,	r,	foo
	r,	two,	tmp,	taken
	zero,	one,	tmp,	after_taken

taken:
	zero,	a,	tmp,	_p3

_p3:
	p,	a,	p,	foo

after_taken:
	tmp,	tmp,	a,	_p4

_p4:
	zero,	a,	a,	foo
	zero,	one,	tmp,	mul

foo:
	zero,	one,	tmp,	0xff
mul_end:
	zero,	one,	tmp,	0xff