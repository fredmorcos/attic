from os import path
from extra.printer import pprint, GREEN, BLUE, RED

class Benchmark(object):
	useCairoPlot = False
	useGnuPlot = True
	
	def __init__(self, plant, orderList, testNumber):
		self.prefix = "generic"
		self.testName = "generic"
		self.testNumber = testNumber
		self.cairoPlotTimes = []
		self.gnuPlotTimes = []
		self.plant = plant
		self.orderList = orderList
		self.orderListSize = -1
		self.machineListSize = -1
		self.startValue = 0

	def addGnuPlotTime(self, x, y):
		self.gnuPlotTimes.append((x, y))
		pprint("PERF Time = " + str(y), GREEN)
		
	def addCairoPlotTime(self, t):
		self.cairoPlotTimes.append(t)
		pprint("PERF Time = " + str(t), GREEN)
		
	def prepare(self):
		pprint("PERF Starting " + self.prefix + " benchmark test " + 
			str(self.testNumber) + " on " + self.testName, BLUE)
		
		if self.orderListSize != -1:
			self.orderList.orders = self.orderList.orders[:self.orderListSize]
			
		if self.machineListSize != -1:
			self.plant.machines = self.plant.machines[:self.machineListSize]
		
		self.times = [i * 0 for i in range(self.startValue)]
	
	def save(self):
		if Benchmark.useCairoPlot == True:
			self.plotCairoPlot()
		
		if Benchmark.useGnuPlot == True:
			self.plotGnuPlot()
			
	def plotGnuPlot(self):
		import os, subprocess
		p = subprocess.Popen(['/usr/bin/gnuplot'], stdin = subprocess.PIPE,
			stdout = subprocess.PIPE, stderr = subprocess.PIPE,	cwd = os.getcwd())
		
		output = ""
		hasFloat = False
		for i in self.gnuPlotTimes:
			if type(i[0]) == float:
				hasFloat = True
			output += str(i[0]) + " " + str(i[1]) + "\n"
		
		with open("plantmaker-tmp", "w") as f:
			f.write(output)
			f.close()
		
		of = "benchmarks/" + self.prefix + "-" + self.testName + "-" + \
			str(self.testNumber) + "-gp.eps"
		commString = "set grid; set term postscript; set out '" + of + "'; " + \
			"set format y \"%.4f\"; " + "set xlabel \"" + self.testName + "\"; " + \
			"set ylabel \"Time (Seconds)\"; unset key; "
		if hasFloat == True:
			commString += "set format x \"%.1f\"; "
		commString += "plot 'plantmaker-tmp' with lines lw 3, 'plantmaker-tmp' with points pt 7 ps 1\n"
			
		p.communicate(commString)
		p.wait()
		os.remove("plantmaker-tmp")
			
	def plotCairoPlot(self):
		try:
			from thirdparty.CairoPlot import dot_line_plot
		except:
			pprint("PERF Will not output to graph. Install CairoPlot.", RED)
			return
			
		dot_line_plot(path.join("benchmarks", self.prefix + "-" + self.testName + 
			"-" + str(self.testNumber)) + ".png",
			self.cairoPlotTimes, 800, 800, (255, 255, 255), 5, True, True, True,
			None, None, None, None)
		
		dot_line_plot(path.join("benchmarks", self.prefix + "-" + self.testName + 
			"-" + str(self.testNumber)) + ".ps",
			self.cairoPlotTimes, 800, 800, (255, 255, 255), 5, True, True, True,
			None, None, None, None)
		
	def run(self):
		self.prepare()
		self.bench()
		self.save()
		
	def bench(self):
		pass
