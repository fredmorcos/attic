#!/usr/bin/python

# Frederic-Gerald Morcos
# Licensed to Public Domain

with open("GS.csv") as f:
	data = f.readlines()

trainfile = open("training.csv", "w")
testfile = open("testing.csv", "w")

trainfile.write(data[0][0:len(data[0]) - 2] + '\n')
testfile.write(data[0][0:len(data[0]) - 2] + '\n')

i = 1
while i < len(data):
	if i == len(data):
		break;
	# else
	trainfile.write(data[i][0:len(data[i]) - 2] + '\n')

	if i + 1 == len(data):
		break;
	# else
	trainfile.writelines(data[i + 1][0:len(data[i + 1]) - 2] + '\n')

	if i + 2 == len(data):
		break;
	# else
	testfile.writelines(data[i + 2][0:len(data[i + 2]) - 2] + '\n')
	
	i += 3

trainfile.close()
testfile.close()

