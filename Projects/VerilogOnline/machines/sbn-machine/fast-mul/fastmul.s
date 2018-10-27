.data
zero: 0, one: 1, two: 2,
a:    3, b:   7,
p:    0, q:   0, n:   0, r: 0,
i:    1, d:   2,
tmp:  0, dx2: 0

.text

mul_start:	b,	one,	tmp,	mul_end
		b,	zero,	n,	foo
		zero,	zero,	q,	foo

half_start:	n,	two,	tmp,	half_end
		two,	zero,	d,	foo
		one,	zero,	i,	foo

inhalf_start:	zero,	d,	dx2,	_p1
_p1:		tmp,	d,	dx2,	_p2
_p2:		n,	tmp,	tmp,	inhalf_end
	
		d,	zero,	i,	foo
		dx2,	zero,	d,	foo
		zero,	one,	tmp,	inhalf_start

inhalf_end:	zero,	i,	tmp,	_p3

_p3:		q,	tmp,	q,	foo
		n,	d,	n,	foo
		zero,	one,	tmp,	half_start

half_end:	q,	zero,	b,	foo
		n,	zero,	r,	foo
	
		two,	r,	tmp,	alternative
		zero,	a,	tmp,	_p4
_p4:		p,	tmp,	p,	foo
alternative:	zero,	a,	tmp,	_p5
_p5:		tmp,	a,	tmp,	_p6
_p6:		zero,	tmp,	a,	foo

		zero,	one,	tmp,	mul_start

foo:		zero,	one,	0xff,	0xff
mul_end:	zero,	one,	0xff,	0xff