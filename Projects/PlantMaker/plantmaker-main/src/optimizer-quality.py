#!/usr/bin/python
import os, subprocess

optimizerbin = ['/usr/bin/python', './src/plantmaker.py', 'optimize', 
                'qualityplant', 'qualityorders', 'qualityconfig']

testname = ''
hasfloat = ''
gnuplotcommand = ''

runs = 10
data = ''

iterations = 0
popsize = 0
mutrange = 0
mutrate = 0.0
selrate = 0.0

def setplotcommand():
  global gnuplotcommand
  gnuplotcommand = "set grid; set term postscript; set out \'%s\'; set xlabel \"%s\"; set ylabel \"Fitness\"; unset key; %s plot \'%s\' with lines lw 3, \'%s\' with points pt 7 ps 1\n" % ('quality/' + testname + '.eps', testname, hasfloat, 'quality/' + testname + '.txt', 'quality/' + testname + '.txt')

def setfloat():
  global hasfloat
  hasfloat = "set format x \"%.1f\"; "
  
def unsetfloat():
  global hasfloat
  hasfloat = ''

def plotgraph():
  print 'Plotting...'
  writefile()
  setplotcommand()
  
  p = subprocess.Popen(['/usr/bin/gnuplot'], stdin = subprocess.PIPE,
      stdout = subprocess.PIPE, stderr = subprocess.PIPE,  cwd = os.getcwd())
  p.communicate(gnuplotcommand)

def writefile():
  with open('quality/' + testname + '.txt', 'w') as f:
    f.write(data)
    f.close()

def run():
  p = subprocess.Popen(optimizerbin, stdout = subprocess.PIPE,
                       stderr = subprocess.PIPE,  cwd = os.getcwd())
  out, err = p.communicate()
  for l in out.splitlines():
    if l.rfind('Fitness: ') != -1:
      return int(l[21:])

def reset():
  global iterations
  global popsize
  global mutrange
  global mutrate
  global selrate
  
  global data
  global testname
  
  iterations = 40
  popsize = 20
  mutrange = 30
  mutrate = 0.5
  selrate = 0.5
  
  data = ''
  testname = ''
  unsetfloat()

def writeconfig():
  data = '''
  <config>
  <evaluator  finishLatePenalty="-2"
              finishExactlyPenalty="2"
              finishEarlyPenalty="-1"
              powerUsageWeight="0"
              craneIdleTimesWeight="0"
              orderDeadlinesWeight="0"
              zincUtilizationWeight="1"
  />
  
  <optimizer  populationSize="%s"
              mutationRange="%s"
              iterations="%s"
              indivMutationRate="%s"
              selectionRate="%s"
  />
  </config>
  ''' % (str(popsize), str(mutrange), str(iterations), str(mutrate), str(selrate))
  
  with open('configs/qualityconfig.xml', 'w') as f:
    f.write(data)
    f.close()

reset()
testname = 'Iterations'
for i in range(1, 42, 4):
  iterations = i
  writeconfig()

  f = 0
  
  for x in range(runs):
    f += run()
  
  f = int(f / runs)
  data += str(iterations) + ' ' + str(f) + '\n'
plotgraph()

reset()
testname = 'PopulationSize'
for i in range(1, 21, 1):
  popsize = i
  writeconfig()

  f = 0
  
  for x in range(runs):
    f += run()
  
  f = int(f / runs)
  data += str(popsize) + ' ' + str(f) + '\n'
plotgraph()

reset()
testname = 'MutationRange'
for i in range(2, 62, 2):
  mutrange = i
  writeconfig()

  f = 0
  
  for x in range(runs):
    f += run()
  
  f = int(f / runs)
  data += str(mutrange) + ' ' + str(f) + '\n'
plotgraph()

reset()
testname = 'MutationRange2'
for i in range(2, 202, 2):
  mutrange = i
  writeconfig()

  f = 0
  
  for x in range(runs):
    f += run()
  
  f = int(f / runs)
  data += str(mutrange) + ' ' + str(f) + '\n'
plotgraph()

reset()
testname = 'MutationRate'
for i in range(2, 11):
  mutrate = i / 10.0
  writeconfig()

  f = 0
  
  for x in range(runs):
    f += run()
  
  f = int(f / runs)
  data += str(mutrate) + ' ' + str(f) + '\n'
plotgraph()

reset()
testname = 'SelectionRate'
for i in range(1, 11):
  selrate = i / 10.0
  writeconfig()

  f = 0
  
  for x in range(runs):
    f += run()
  
  f = int(f / runs)
  data += str(selrate) + ' ' + str(f) + '\n'
plotgraph()

