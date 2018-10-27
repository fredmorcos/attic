#!/usr/bin/python
'''	
 	This file is part of pyfuzz.
 
 	Copyright 2009	Frederic Morcos <fred.morcos@gmail.com>
 
 	pyfuzz is free software: you can redistribute it and/or modify
 	it under the terms of the GNU General Public License as published by
 	the Free Software Foundation, either version 3 of the License, or
 	(at your option) any later version.
 
 	pyfuzz is distributed in the hope that it will be useful,
 	but WITHOUT ANY WARRANTY; without even the implied warranty of
 	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 	GNU General Public License for more details.
 
 	You should have received a copy of the GNU General Public License
 	along with pyfuzz.  If not, see <http://www.gnu.org/licenses/>.
'''

from ui import UI
from flc import FLC
from variable import Variable
from function import Function

# front side sensor (input)
fssLow = Function("Low")
fssLow.add_point([0.0, 1.0])
fssLow.add_point([10.0, 1.0])
fssLow.add_point([15.0, 0.0])

fssMed = Function("Medium")
fssMed.add_point([10.0, 0.0])
fssMed.add_point([15.0, 1.0])
fssMed.add_point([25.0, 1.0])
fssMed.add_point([30.0, 0.0])

fssHig = Function("High")
fssHig.add_point([25.0, 0.0])
fssHig.add_point([30.0, 1.0])

fss = Variable("Front Side Sensor")
fss.add_function(fssLow)
fss.add_function(fssMed)
fss.add_function(fssHig)

# back side sensor (input)
bssLow = Function("Low")
bssLow.add_point([0.0, 1.0])
bssLow.add_point([20.0, 1.0])
bssLow.add_point([40.0, 0.0])

bssMed = Function("Medium")
bssMed.add_point([20.0, 0.0])
bssMed.add_point([40.0, 1.0])
bssMed.add_point([60.0, 0.0])

bssHig = Function("High")
bssHig.add_point([40.0, 0.0])
bssHig.add_point([60.0, 1.0])

bss = Variable("Back Side Sensor")
bss.add_function(bssLow)
bss.add_function(bssMed)
bss.add_function(bssHig)

# wheel speed (output)
wsLow = Function("Low")
wsLow.add_point([20.0, 0.0])
wsLow.add_point([30.0, 1.0])
wsLow.add_point([40.0, 0.0])

wsMed = Function("Medium")
wsMed.add_point([40.0, 0.0])
wsMed.add_point([50.0, 1.0])
wsMed.add_point([55.0, 0.0])

wsHig = Function("High")
wsHig.add_point([55.0, 0.0])
wsHig.add_point([60.0, 1.0])
wsHig.add_point([70.0, 0.0])

ws = Variable("Wheel Speed")
ws.add_function(wsLow)
ws.add_function(wsMed)
ws.add_function(wsHig)

# steering (output)
sLef = Function("Left")
sLef.add_point([-180.0, 0.0])
sLef.add_point([-90.0, 1.0])
sLef.add_point([-40.0, 0.0])

sZer = Function("Zero")
sZer.add_point([-40.0, 0.0])
sZer.add_point([-0.0, 1.0])
sZer.add_point([40.0, 0.0])

sRig = Function("Right")
sRig.add_point([40.0, 0.0])
sRig.add_point([90.0, 1.0])
sRig.add_point([180.0, 0.0])

s = Variable("Steering")
s.add_function(sLef)
s.add_function(sZer)
s.add_function(sRig)

# the fuzzy logic controller
flc = FLC("Right-Edge Following Behavior of Mobile Robot")
flc.add_input(fss)
flc.add_input(bss)
flc.add_output(ws)
flc.add_output(s)

flc.generate_rulebase()

# ui stuff
ui = UI(flc)
ui.showUI()
