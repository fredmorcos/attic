#!/usr/bin/python
'''	
 	This file is part of pynn.
 
 	Copyright 2009	Frederic Morcos <fred.morcos@gmail.com>
 
 	pynn is free software: you can redistribute it and/or modify
 	it under the terms of the GNU General Public License as published by
 	the Free Software Foundation, either version 3 of the License, or
 	(at your option) any later version.
 
 	pynn is distributed in the hope that it will be useful,
 	but WITHOUT ANY WARRANTY; without even the implied warranty of
 	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 	GNU General Public License for more details.
 
 	You should have received a copy of the GNU General Public License
 	along with pynn.  If not, see <http://www.gnu.org/licenses/>.
'''

import random
import math

numHiddenNeurons = 10	# number of neurons in hidden layer
learnRate = 0.5			# learning rate (eita)
momentum = 0.5			# momentum (alpha)
sigmoidLambda = 1.0		# lambda, sigmoid's coefficient
lambdaCoef = 0.5		# lambda coefficient for error calculations
trainFilename = None	# training filename
testFilename = None		# testing filename
training = True			# training/testing state
nepochs = 200			# number of epochs

def sigmoid(val):
	''' sigmoid function '''
	return 1.0 / (1.0 + (math.e ** ((-sigmoidLambda) * val)))

def rand():
	''' returns a random number between 0 and 1 '''
	return random.uniform(0.0, 1.0)

class Node:
	''' neural network node implementation '''
	def __init__(self):
		self.value = 0.0	# node output value

class Neuron(Node):
	''' neural network neuron implementation '''
	def __init__(self):
		Node.__init__(self)
		self.weights = []		# list of synapse weights
		self.dwold = []			# list of old delta weights

class MLP:
	''' multi-layer perceptron implementation '''
	def __init__(self):
		# list of neurons in network, first neuron is input, last neuron is output,
		# all neurons in the middle are the hidden layer nodes
		self.neurons = [Node()]

		# add neurons to network
		tmp = numHiddenNeurons + 1	# + 1 to add the output neuron too
		while tmp > 0:
			self.neurons.append(Neuron())
			tmp -= 1

		# add one synapse (coming from input node) to each hidden neuron
		for n in self.neurons[1:-1]:	# all neurons excluding first and last
			n.weights.append(rand())
			n.dwold.append(0.0)

		# add synapses to the output neuron
		tmp = numHiddenNeurons
		while tmp > 0:
			self.neurons[-1].weights.append(rand())
			self.neurons[-1].dwold.append(0.0)
			tmp -= 1

	def startIteration(self, sampleInput, sampleOutput):
		''' run a single iteration with sample input and output, returns error '''
		for n in self.neurons:					# reset all neuron values to 0
			n.value = 0.0

		self.neurons[0].value = sampleInput		# set input to network
		self.calcHiddenNeuronValues()			# calculate hidden layer neuron values
		self.calcOutputNeuronValue()			# calculate output layer neuron values

		return (sampleOutput - self.neurons[-1].value)	# return diff/err

	def calcOutputNeuronValue(self):
		''' calculates the value of the output neuron '''
		n = self.neurons[-1]					# output neuron n
		hidden = self.neurons[1:-1]				# hidden neurons
		assert len(hidden) == len(n.weights)
		for i, w in enumerate(n.weights):		# for each weight w in n
			n.value += (w * hidden[i].value)	# sum(weights * val)
		n.value = sigmoid(n.value)			# FIXME ???

	def calcHiddenNeuronValues(self):
		''' calculate the values of the hidden layer neurons '''
		for n in self.neurons[1:-1]:					# for each hidden neuron n
			for w in n.weights:							# for each weight w in n
				n.value += (w * self.neurons[0].value)	# sum(weights * val)
			n.value = sigmoid(n.value)					# val = sigmoid(val)
	
	def backpropagationOutput(self, e):
		''' runs the back-propagation algorithm for the output neuron and synapses '''
		hidden = self.neurons[1:-1]								# hidden neurons
		assert len(hidden) == len(self.neurons[-1].dwold)
		assert len(hidden) == len(self.neurons[-1].weights)
		doutput = lambdaCoef * self.neurons[-1].value * (1 - self.neurons[-1].value) * e

		for i, w in enumerate(self.neurons[-1].weights):		# update all output weights
																# calc delta weight
			dw = (learnRate * doutput * hidden[i].value) + \
				 (self.neurons[-1].dwold[i] * momentum)
			w += dw												# update weight
			self.neurons[-1].dwold[i] = dw						# set old delta weight
	
	def backpropagationHidden(self, e):
		''' runs the back-propagation algorithm for the synapses in the hidden layer '''
		hidden = self.neurons[1:-1]
		for hn in hidden:											# for each hidden neuron hn
			doutput = lambdaCoef * hn.value * (1 - hn.value) * e	# FIXME ???
			# doutput = lambdaCoef * hn.value * (1 - hn.value) * \
			#		  (hn.weights[0] * self.neurons[0].value)
			assert len(hn.weights) == 1
			assert len(hn.dwold) == 1
																	# delta weight
			dw = (learnRate * doutput * self.neurons[0].value) + \
				 (hn.dwold[0] * momentum)
			hn.weights[0] += dw										# update weight
			hn.dwold[0] = dw										# set old delta weight

class Network(MLP):
	''' network implementation to manage input files from training and test, epochs,
		training/testing states, creation of perceptron, etc... '''
	def __init__(self):
		MLP.__init__(self)
	
	def startEpoch(self, sampleData):
		''' start the training process using each input for 1 epoch only '''
		epochErrors = []
		avg = 0.0

		for sd in sampleData:
			e = self.startIteration(sd[0], sd[1])	# run on samples and get error
			if training is True:					# run back-propagation algorithm
				self.backpropagationOutput(e)
				self.backpropagationHidden(e)
			epochErrors.append(e)

		for err in epochErrors:						# calc average epoch error
			avg += (err ** 2)						# 0.5 * sum(e^2)

		# return math.sqrt((0.5 * avg) / len(epochErrors))
		return ((0.5 * avg) / len(epochErrors))
	
	def start(self):
		''' loads the files, creates the inputs and outputs and runs the epochs '''
		sampleData = []
		epochErrors = []

		if training is True:						# open file into list of lines
			data = open(trainFilename).readlines()
		else:
			data = open(testFilename).readlines()

		data = data[1:]								# ignore first line "Inp1,Out1"
		for d in data:								# for each line in data
			tmp = d.split(',')						# split at ,
			input = float(tmp[0])					# first col is input
			input = (input + 180.0) / 360.0			# map to [0, 1]
			output = float(tmp[1])					# second col is ouput
			output = (output + 50.0) / 150.0		# map to [0, 1]
			sampleData.append((input, output))		# add input and output to sample data

		i = nepochs									# run epochs with sample data
		while i > 0:
			epochErrors.append(self.startEpoch(sampleData))
			i -= 1
		return epochErrors

# trainFilename = 'training.csv'
# testFilename = 'testing.csv'
# network = Network()
# training = True
# errs = network.start()
# print errs

