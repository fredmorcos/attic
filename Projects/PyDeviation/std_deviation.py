#!/usr/bin/python

import math

def standard_deviation(list):
	mean = 0
	stdvar = 0
	squares = []

	for i in list:
		mean = mean + i
	mean = mean / len(list)

	for i in list:
		squares.append((i - mean) ** 2)

	for i in squares:
		stdvar = stdvar + i
	stdvar = stdvar / len(squares)
	stdvar = math.sqrt(stdvar)

	return mean, stdvar
