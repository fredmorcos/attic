#!/usr/bin/python

# triphone_generator.py
# Frederic Morcos <fred.morcos@gmail.com>
# Public Domain

import sys

verbose = False
filename = None
show_help = False

for i in sys.argv[1:]:
	# check the arguments for --options,
	# --help, filename, etc... and set the
	# corresponding variables
	if i == '--help':
		show_help = True
	else:
		filename = i

if show_help:
	print 'triphone generator script'
	print 'Frederic Morcos <fred.morcos@gmail.com>'
	print 'released to the public domain'
	print ''
	print './triphone_generator.py <filename> [--help]'
	print ''
	print 'arguments:'
	print '--help\t\t\tshow this help'

	exit(0)

def print_triphones(x):
	print 'number of triphones =', len(x)
	for i in x:
		for j in i:
			print j,
		print ''

f = file(filename)					# open the phonemes file

phonemes = []						# list of word phonemes
triphones = []						# list of result triphones
triphones_nodup = []				# list without duplicates

while True:
	line = f.readline()

	if line == '':					# if EOF, break
		break

	tmp = line.split()				# split line at spaces
	phonemes.append(tmp[1:])

# finished parsing file

for i in phonemes:					# find internal triphones
	for index, j in enumerate(i):
		tmp = i[index:index + 3]

		if len(tmp) < 3:
			break

		triphones.append(tmp)

for i in phonemes:					# find external triphones
	new_triphone1 = []				# triphone with 1st phoneme and
									# last 2 from all other words
	new_triphone2 = []				# triphones with 1st 2 phonemes
									# and last 1 from all other words

	new_triphone1.append(i[0])
	new_triphone2.append(i[0] + ' ' + i[1])

	for j in phonemes:
		triphones.append(j[len(j) - 2:] + new_triphone1)
		triphones.append(j[len(j) - 2:] + ['SIL'])
		triphones.append(j[len(j) - 1:] + new_triphone2)
		triphones.append(['SIL'] + new_triphone2)

for i in triphones:
	if i not in triphones_nodup:
		triphones_nodup.append(i)

print_triphones(triphones_nodup)
