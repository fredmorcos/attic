import logging as log
import modules.extra as e

from modules.extra import die

def _machineMemWrite(machine, data):
    # Executes a memwrite instruction/command. Finds the appropriate memory
    # element and writes to it the data.
    for k,v in machine.get('mem_elements', {}).items():
        if v['name'] == data[0]:
            # mem[address] = data
            v['value'][int(data[1], 16)] = data[2]

    for k,v in machine.get('sub_elements', {}).items():
        _machineMemWrite(v, data)

def _machineUnmarkAll(machine):
    # Unmarks all element and sub-components in this component. In other words,
    # resets all the 'changed' markers as to not color any components when at the
    # rendering stage, this is useful for the very first render (clock cycle 0).
    for k,v in machine.get('elements', {}).items():
        v['changed'] = False

    for k,v in machine.get('sub_elements', {}).items():
        _machineUnmarkAll(v)

def _machineLoadMem(machine):
    # Loads memory data from respective files (generated from the assembler stage)
    # into the appropriate memories in the machine.
    s = e.settings
    d = s.__dict__

    for k,v in machine.get('mem_elements', {}).items():
        name = v['name']

        if name == 'PROG' and 'program_prog' in d:
            with open(s.program_prog, 'r') as f: v['value'] = f.readlines()
        elif name == 'DPROG' and 'program_dprog' in d:
            with open(s.program_dprog, 'r') as f: v['value'] = f.readlines()
        elif name == 'DATA' and 'program_data' in d:
            with open(s.program_data, 'r') as f: v['value'] = f.readlines()
        elif name == 'DDATA' and 'program_ddata' in d:
            with open(s.program_ddata, 'r') as f: v['value'] = f.readlines()

    for k,v in machine.get('sub_elements', {}).items():
        _machineLoadMem(v)

def _machineLoadValues(machine, values):
    # Puts values generated at the simulation level into their respective components.
    # Updates values of elements and sets the change flag accordingly.
    for k,v in machine.get('elements', {}).items():
        if v['value'] != values[v['index']]:
            v['value']   = values[v['index']]
            v['changed'] = True

    for k,v in machine.get('sub_elements', {}).items():
        _machineLoadValues(v, values)

class Emulator:
    # This class is used to parse simulation output to find a certain posedge
    # clock cycle. In other words, a clock cycle emulator.
    def __init__(self, machine, sim_data):
        # Machine is the simulation's machine.
        # SimData cannot be empty.
        self.machine  = machine
        if sim_data.strip() == '': die('empty sim data')
        self.data = sim_data.splitlines()
        self.step = e.settings.step

    def _checkCycleLine(self, line):
        # Some sanity checks on split line (that time and clk are integers).
        try:    time = int(line[0])
        except: die('first col (time) in sim output not an int decimal')

        try:    clk = int(line[1], 2)
        except: die('second col (clk) in sim output not a binary decimal')

        log.info('emulator pass, time=%s, clk=%s' % (time, clk))
        return time, clk

    def advance(self, step=0):
        # Moves through the simulation data, parsing every line. Works
        # according to the emulator state machine:
        # START: Get 0 -> S0
        # S0:    Get 0 -> S0
        # S0:    Get 1 -> S1
        # S1:    Get 1 -> S1
        # S1:    Get 0 -> S0, step++, machine.load_values()
        # END when counter == wanted_steps or premature_finish
        # Returns True for success
        # Returns False for premature finish
        splitLine = lambda row: self.data[row].strip().split(':')

        if len(self.data) == 0: die('empty sim data')
        if step < 0:            die('bad step number %s' % step)

        if step == 0:
            tmpLine = splitLine(0)

            if tmpLine[0] == 'mw': die('step 0 is a memwrite operation')

            self._checkCycleLine(tmpLine)
            _machineLoadMem(self.machine)
            _machineLoadValues(self.machine, tmpLine[2].split(','))
            _machineUnmarkAll(self.machine)
        else:
            i = 0
            row = 0
            state = 0

            _machineLoadMem(self.machine)

            while i < step:
                if row == len(self.data) - 1: return False

                _machineUnmarkAll(self.machine)
                tmpLine = splitLine(row)

                if tmpLine[0] == 'mw':
                    _machineMemWrite(self.machine, tmpLine[1].split(','))
                else:
                    time, clk = self._checkCycleLine(tmpLine)

                    if state == 0 and clk == 1: state = 1
                    elif state == 1 and clk == 0:
                        state = 0; i += 1
                        pLine = splitLine(row-1) # previous line
                        log.info('applying line at time=%s, clk=%s' %
                                 (pLine[0], pLine[1]))
                        _machineLoadValues(self.machine, pLine[2].split(','))
                row += 1
        return True
