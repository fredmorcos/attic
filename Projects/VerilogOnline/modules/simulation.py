import os
import shutil

import logging as log
import modules.extra as e

from os.path import relpath

from modules.extra import die
from modules.extra import mktempf
from modules.extra import execProcess

class Simulation:
    # This class represents a simulation wrapper. According to
    # docs/simulation.txt, there are two types, verilog-like where the
    # simulation consists of a simulator and a machine description file
    # (verilog code), or, a standalone binary that simulates a machine.
    def __init__(self, machine):
        # machine is the machine information to simulate.
        # data is the output of the simulation (usually to the traverser).
        self.machine  = machine
        self.data     = None
        self.error    = None
        self.run()

    def _findSimulatorBinary(self):
        # This function finds the simulator binary, whether it is veriwell or
        # other (also possibly standalone). It uses the binary filename as a
        # system-command string.
        log.info('looking for simulator binary')
        simBin = self.machine['simulator']

        if not os.path.exists(simBin):
            log.warning('sim binary %s does not exist' % relpath(simBin))
        log.info('using %s from system as simulator bin' % relpath(simBin))

        return simBin

    def _checkSplitSimOutput(self):
        # Check sanity of the simulator output and splits it accordingly.
        sInd = '=== start ==='
        eInd = '=== end ==='

        if sInd not in self.data: die('start-ind not found in sim output')
        if eInd not in self.data: die('end-ind not found in sim output')

        if self.data.index(eInd) < self.data.index(sInd):
            die('end-ind found before start-ind in sim output')

        self.data = self.data.split(sInd)[1].split(eInd)[0].strip()

    def _runVeriwellSim(self):
        # This function runs a veriwell simulation, it dumps the machine
        # description file and runs the user's program/data files on it.
        veriwellBin = self._findSimulatorBinary()

        # sc_d = simcode_data
        # sc_f = simcode_file
        sc_d = self.machine['simcode']
        sc_d = sc_d.replace('%MAX_STEPS%', str(e.settings.maxSteps))
        sc_d = sc_d.replace('%PROG%', '"%s"' % e.settings.program_prog)

        if not self.machine['single_mem']:
            sc_d = sc_d.replace('%DATA%', '"%s"' % e.settings.program_data)

        sc_f = mktempf('_user_sc')
        with open(sc_f, 'w') as f: f.write(sc_d)
        e.settings.simcode_file = sc_f

        p_cmdline = [veriwellBin, '-l', e.settings.simLogFile, '-k',
                     os.devnull, sc_f]
        self.data = execProcess(p_cmdline, 'simulator',
                                showOut=False, showErr=False, canDie=True)[0]
        self._checkSplitSimOutput()

    def _runStandaloneSim(self):
        # This functions runs a standalone simulation binary, pass the user
        # program/data files as command line arguments.
        log.info('sim type: standalone')
        simBin = self._findSimulatorBinary()
        log.info('running standalone simulation')
        p_cmdline = [simBin, '--input', e.settings.program_prog]
        if not self.machine['single_mem']:
            p_cmdline += [e.settings.program_data]
        self.data, self.error, rc = execProcess(p_cmdline, 'simulator',
                                                e.settings, canDie=True)
        self._checkSplitSimOutput()

    def run(self):
        # This is the main flowchart-like logic to deal with possible
        # cases of missing machine file and/or simulator
        # binary. Described in docs/simulation.dia.
        simcode = self.machine['simcode']
        simulator = self.machine['simulator']

        if not simcode:
            log.info('sim machine is empty, checking simulator')

            if not simulator: die('no simulator nor machine provided')
            else:
                log.info('simulator not empty, checking')

                if simulator == 'veriwell': die('no machine for verilog sim')
                elif simulator == 'vhdl':   die('no machine for vhdl sim')
                else: self._runStandaloneSim()
        else:
            log.info('sim machine not empty, checking simulator')

            if not simulator:
                log.info('simulator empty, checking extension')

                if simcode.endswith('.v'):
                    log.info('machine ends in .v, using veriwell')
                    self._runVeriwellSim()
                elif simcode.endswith('.vh'): # put other simulators here
                    log.info('machine ends in .vh, using vhdl')
                    # run vhdl simulation
                else: die('cannot determine simulator from extension')
            else:
                log.info('simulator not empty, checking')

                if simulator == 'veriwell':
                    log.info('machine not empty, simulator is veriwell')
                    self._runVeriwellSim()
                elif simulator == 'vhdl': # put other simulators here
                    log.info('machine not empty, simulator is vhdl')
                else: die('unknown simulator given')
