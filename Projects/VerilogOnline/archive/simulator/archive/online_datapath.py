from pyjamas.ui.Button import Button
from pyjamas.ui.Label import Label
from pyjamas.ui.RootPanel import RootPanel
from pyjamas.ui.VerticalPanel import VerticalPanel
from pyjamas.ui.HorizontalPanel import HorizontalPanel
from pyjamas.Canvas2D import Canvas

class Obj:
	def __init__(self, name):
		self.name = name
		self.currentValue = 0
		self.newValue = 0

	def reset(self):
		self.currentValue = 0
		self.newValue = 0

	def update(self):
		self.currentValue = self.newValue
		self.newValue = 0

class Edge:
	def __init__(self, name, n1, n2):
		self.name = name
		self.n1 = n1
		self.n2 = n2
		self.value = 0
		self.x1 = n1.x2
		self.x2 = n2.x1
		self.y1 = n1.y1 + (n1.size[1] / 2)
		self.y2 = n2.y1 + (n2.size[1] / 2)

	def draw(self, context):
		context.strokeStyle = 'black'
		context.moveTo(self.x1, self.y1)
		context.lineTo(self.x2, self.y2)
		context.stroke()

	def reset(self):
		self.value = 0

class Node:
	def __init__(self, obj, size = (100, 100), pos = (100, 100)):
		self.obj = obj
		self.size = size
		self.x1 = pos[0]
		self.y1 = pos[1]
		self.x2 = pos[0] + size[0]
		self.y2 = pos[1] + size[1]

	def draw(self, context):
		context.strokeStyle = 'black'
		context.fillStyle = 'white'
		context.strokeRect(self.x1, self.y1, self.size[0], self.size[1])
		context.fillRect(self.x1, self.y1, self.size[0], self.size[1])

class DatapathCanvas(Canvas):
	def __init__(self, nameLabel, currentValueLabel, newValueLabel):
		Canvas.__init__(self, 800, 300)
		self.nodes = []
		self.edges = []

		self.nameLabel = nameLabel
		self.currentValueLabel = currentValueLabel
		self.newValueLabel = newValueLabel

		self.addMouseListener(self)
		self.draw()

	# this is just a quick hack, should be rewritten
	# properly to support per-node redrawing
	def draw(self):
		self.context.strokeStyle = 'black'
		self.context.strokeRect(0, 0, 800, 300)

		for n in self.nodes:
			n.draw(self.context)

		for e in self.edges:
			e.draw(self.context)

	def addNode(self, node):
		self.nodes.append(node)
		self.draw()

	def addEdge(self, edge):
		self.edges.append(edge)
		self.draw()

	# this should be rewritten
	def getItemAt(self, x, y):
		for n in self.nodes:
			if n.x1 <= x and n.y1 <= y and n.x2 >= x and n.y2 >= y:
				return n

		for e in self.edges:
			if e.x1 - 10 <= x and e.y1 - 10 <= y and e.x2 + 10 >= x \
			   and e.y2 + 10 >= y:
				return e

		return None

	# this should also be rewritten
	# this whole thing should be rewritten
	def onMouseMove(self, sender, x, y):
		item = self.getItemAt(x, y)
		if item is None:
			self.nameLabel.setText('')
			self.currentValueLabel.setText('')
			self.newValueLabel.setText('')
			return

		if isinstance(item, Node):
			self.nameLabel.setText('Name: ' + item.obj.name)
			self.currentValueLabel.setText('Current Value: %s' % str(item.obj.currentValue))
			self.newValueLabel.setText('New Value: %s' % str(item.obj.newValue))
		else:
			self.newValueLabel.setText('')
			self.nameLabel.setText('Name: ' + item.name)
			self.currentValueLabel.setText('Value: %s' % (item.value))

### datapath nodes and program ###
'''
why are these here? because we need to declare and define the classes
to be able to instantiate them. then we need to declare the global variables
before we use them in the functions below.
'''
pc = Obj('PC')
imem = Obj('IMem')
ir = Obj('IR')

n1 = Node(pc)
n2 = Node(imem, pos = (300, 100))
n3 = Node(ir, pos = (500, 100))

addr = Edge('Addr', n1, n2)
ins = Edge('Ins', n2, n3)

PROGRAM = '''ins0
ins1
ins2
ins3
'''.split()

### end of datapath nodes and program ###

def doReset():
	pc.reset()
	imem.reset()
	ir.reset()

	addr.reset()
	ins.reset()

	imem.currentValue = PROGRAM

def doStep():
	if pc.currentValue >= len(PROGRAM):
		doReset()
		return

	addr.value = pc.currentValue
	ins.value = imem.currentValue[addr.value]
	ir.newValue = ins.value
	pc.newValue = pc.currentValue + 1
	
	pc.update()
	ir.update()

if __name__ == '__main__':
	mainPanel = VerticalPanel()
	controlPanel = HorizontalPanel()
	
	debugLabel = Label('')

	nameLabel = Label('')
	currentValueLabel = Label('')
	newValueLabel = Label('')
	resetButton = Button('Reset', doReset)
	stepButton = Button('Step', doStep)
	
	datapathCanvas = DatapathCanvas(nameLabel, currentValueLabel, newValueLabel)

	datapathCanvas.addNode(n1)
	datapathCanvas.addNode(n2)
	datapathCanvas.addNode(n3)

	datapathCanvas.addEdge(addr)
	datapathCanvas.addEdge(ins)

	controlPanel.setSpacing(20)
	controlPanel.add(resetButton)
	controlPanel.add(stepButton)
	controlPanel.add(nameLabel)
	controlPanel.add(currentValueLabel)
	controlPanel.add(newValueLabel)
	
	mainPanel.add(datapathCanvas)
	mainPanel.add(controlPanel)
	mainPanel.add(debugLabel)
	
	RootPanel().add(mainPanel)

	doReset()

