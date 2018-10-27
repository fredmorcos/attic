#!/usr/bin/python

# phoneme_counter.py
# Frederic Morcos <fred.morcos@gmail.com>
# Public Domain

import sys

purge_underscore = False
verbose = False
filename = None
show_help = False

for i in sys.argv[1:]:
	# check the arguments for --options, 
	# --help, filename, etc... and set the 
	# corresponding variables
	if i == '--purge-underscore':
		purge_underscore = True
	else:
		if i == '--verbose':
			verbose = True
		else:
			if i == '--help':
				show_help = True
			else:
				filename = i

if show_help:
	print 'phonemes counter script'
	print 'works on the wsj and tidigits phoneme lists'
	print 'Frederic Morcos <fred.morcos@gmail.com>'
	print 'released to the public domain'
	print ''
	print './phoneme_counter.py <filename> [--purge-underscore] [--verbose] [--help]'
	print ''
	print 'formats:'
	print '1 - word\tW_word O_word R_word D_word'
	print '2 - word\tW O R D'
	print ''
	print 'arguments:'
	print '--purge-underscore\tfor format 1'
	print '--help\t\t\tshow this help'
	print '--verbose\t\tprint the final list of phonemes'
	
	exit(0)

# open the phonemes file
f = file(filename)

# phonemes list with repetitions
tmp_phon = []
# phonemes list without repetitions
phonemes = []

while True:
	# go over each line, we have 2 formats:
	# 1 - word		W_word O_word R_word D_word
	# 2 - word      W O R D
	# where the capitals are the phonemes
	line = f.readline()
	# if EOF, break
	if line == '':
		break
	# split the current line from spaces
	tmp = line.split()
	# ditch the first word which is 'word' in
	# the format described above
	for i in tmp[1:]:
		# if the format is #1, we need to remove 
		# the extra '_word' from each phoneme
		# because --purge_underscore has been set
		if purge_underscore:
			tmp2 = i.split('_')[0]
		else:
			tmp2 = i
		# add the resulting phoneme to the list 
		# of phonemes
		tmp_phon.append(tmp2)

# remove duplicates
for i in tmp_phon:
	if i not in phonemes:
		phonemes.append(i)

if verbose:
	print phonemes
print 'number of phonemes:', len(phonemes)

