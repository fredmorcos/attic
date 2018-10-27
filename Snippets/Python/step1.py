#!/usr/bin/python

# Frederic-Gerald Morcos
# Public Domain, with credit reference.

maxi = 17 # numbering of files

with open('testing.csv') as f:
	tmp = f.readlines()

# outf = open('out-all.txt', 'w')

i = 1
while i <= maxi:
	with open(str(i) + '-op') as g:
		outputData = g.readlines()
	
	with open(str(i) + '-err') as h:
		errorData = h.readlines()

	outputData = outputData[0][1:len(outputData[0]) - 2]
	errorData = errorData[0][1:len(errorData[0]) - 2]

	outputData = outputData.split(' ')
	errorData = errorData.split(' ')

	outf = open(str(i) + '-results', 'w')

	outf.write('input'.ljust(15) + 'output'.ljust(15) + 'matlab-output'.ljust(15) + 
			'matlab-error'.ljust(15) + 'calc-error'.ljust(15) + '\n')

	avgMatlabErr = 0
	avgErr = 0
	num = 0

	for index, line in enumerate(tmp[1:]):
		inNum = float(line.split(',')[0])
		outNum = float(line.split(',')[1])

		avgMatlabErr += abs(float(errorData[index]))
		avgErr += abs(abs(float(outputData[index])) - abs(inNum))

		outf.write(str(inNum).ljust(15) + str(outNum).ljust(15) + 
				outputData[index].ljust(15) + errorData[index].ljust(15) + 
				str(abs(abs(float(outputData[index])) - abs(inNum))).ljust(15) + '\n')

		num = index

	outf.write('Average Matlab Error: ' + str(avgMatlabErr / num) + '\n')
	outf.write('Average Calculated Error: ' + str(avgErr / num) + '\n\n\n')

	outf.close()

	i += 1
	
# outf.close()

