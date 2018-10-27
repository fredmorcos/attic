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

import network
from network import Network

import gtk
from gtkcairoplot import gtk_function_plot

class UI:
	def __init__(self):
		self.errors = []
		self.network = None
		
		builder = gtk.Builder()
		builder.add_from_file("ui.ui")
		
		self.graph = gtk_function_plot()
		
		mainWindow = builder.get_object("mainWindow")
		mainWindow.maximize()
		mainWindow.connect("delete-event", gtk.main_quit)

		mainBox = builder.get_object("mainBox")
		mainBox.pack_start(self.graph)
		
		self.hiddenSpin = builder.get_object("hiddenSpin")
		self.epochSpin = builder.get_object("epochSpin")
		self.lambdaSpin = builder.get_object("lambdaSpin")
		self.eitaSpin = builder.get_object("eitaSpin")
		self.alphaSpin = builder.get_object("alphaSpin")
		self.updateSpin = builder.get_object("updateSpin")
		
		self.createButton = builder.get_object("createButton")
		self.trainButton = builder.get_object("trainButton")
		self.runButton = builder.get_object("runButton")
		
		self.createButton.connect("clicked", self.create)
		self.trainButton.connect("clicked", self.train)
		self.runButton.connect("clicked", self.test)
		
		self.graph.set_args({'data':self.plotErrors, 'grid':True, 
							'x_bounds':(0, network.nepochs), 'step':1,
							'x_title':'Number of Epochs', 'y_title':'Average Epoch Error',
							'background':'white white', 'series_colors':['blue']});
		
		mainWindow.show_all()
		gtk.main()
		
	def plotErrors(self, x):
		if x > len(self.errors) - 1:
			if len(self.errors) is 0:
				return 0
			else:
				return self.errors[-1]
		else:
			return self.errors[x]

	def create(self, widget):
		self.config()
		self.network = Network()
		network.trainFilename = 'training.csv'
		network.testFilename = 'testing.csv'

	def train(self, widget):
		network.training = True
		self.errors = []
		self.errors = self.network.start()
		self.graph.set_args({'data':self.plotErrors, 'grid':True, 
							'x_bounds':(0, network.nepochs), 'step':1,
							'x_title':'Number of Epochs', 'y_title':'Average Epoch Error',
							'background':'white white', 'series_colors':['blue']});
		self.graph.queue_draw()

	def test(self, widget):
		network.training = False
		self.errors = []
		self.errors = self.network.start()
		self.graph.set_args({'data':self.plotErrors, 'grid':True, 
							'x_bounds':(0, network.nepochs), 'step':1,
							'x_title':'Number of Epochs', 'y_title':'Average Epoch Error',
							'background':'white white', 'series_colors':['blue']});
		self.graph.queue_draw()
	
	def config(self):
		network.numHiddenNeurons = self.hiddenSpin.get_value_as_int()
		network.nepochs = self.epochSpin.get_value_as_int()
		network.learnRate = float(self.eitaSpin.get_value())
		network.momentum = float(self.alphaSpin.get_value())
		network.sigmoidLambda = float(self.lambdaSpin.get_value())
		network.lambdaCoef = float(self.updateSpin.get_value())
		
		print "Training:", network.training
		print "Hidden Neurons:", network.numHiddenNeurons
		print "Num Epochs:", network.nepochs
		print "Eita:", network.learnRate
		print "Alpha:", network.momentum
		print "Sigmoid Lambda:", network.sigmoidLambda
		print "Lambda:", network.lambdaCoef

