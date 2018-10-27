import os, shutil
from os import path
from os.path import relpath
from exceptions import OSError
from subprocess import Popen, PIPE

class Simulation:
    """
    This class represents a simulation wrapper. According to docs/simulation.txt,
    there are two types, verilog-like where the simulation consists of a simulator
    and a machine description file (verilog code), or, a standalone binary that
    simulates a machine.
    """
    def __init__(self, machine):
        """
        Machine is the machine information to simulate.
        Logger is a shortcut to the global logging facility.
        LogFile is the output of the simulation (taken from command line).
        SimulationData is the output of the simulation (usually to the traverser).
        """
        self.machine = machine
        self.logger = self.machine.settings['logger']
        self.logfile = self.machine.settings['sim_log']
        self.data = None
        self.error = None

    def find_machine_file(self):
        """
        This function finds the machine's hardware description file, first looks
        in the machine's definition directory, if not found, looks in data/machines.
        """
        self.logger.info('looking for machine file')
        machine_file = path.join(self.machine.settings['definitions'],
                                 self.machine.settings['machine'],
                                 self.machine.machine)
        if not os.path.exists(machine_file):
            self.logger.warning('machine file %s does not exist' % machine_file)
            machine_file = path.join(self.machine.settings['machines'],
                                     self.machine.machine)
            if not os.path.exists(machine_file):
                self.logger.warning('machine file %s does not exist' % machine_file)
                self.logger.info('cannot find machine file %s, dying' % machine_file)
                self.logger.die('cannot find machine file')
        self.logger.info('machine file found at %s' % machine_file)
        return machine_file

    def find_simulator_binary(self):
        """
        This function finds the simulator binary, whether it is veriwell or other
        (also possibly standalone). It first looks in bin/simulators, if not found,
        uses a system-command string.
        """
        self.logger.info('looking for simulator binary')
        simulator_bin = path.join(self.machine.settings['simulators'],
                                  self.machine.simulator)
        if not os.path.exists(simulator_bin):
            self.logger.warning('simulator bin %s does not exist' % relpath(simulator_bin))
            simulator_bin = self.machine.simulator
        self.logger.info('using %s from system as simulator bin' % relpath(simulator_bin))
        return simulator_bin

    def run_veriwell_simulation(self):
        """
        This function runs a veriwell simulation, it keeps a user-specific copy of the
        machine description file and runs the user's program/data files on it.
        """
        machine_file = self.find_machine_file()
        veriwell_bin = self.find_simulator_binary()
        user_machine_file = os.tempnam()
        shutil.copyfile(machine_file, user_machine_file)
        self.machine.settings['user_machine_file'] = user_machine_file

        with open(user_machine_file, 'r') as f: tmp_data = f.read()
        tmp_data = tmp_data.replace('%PROG%', '"%s"' % self.machine.settings['program_prog'])
        if not self.machine.single_mem:
            tmp_data = tmp_data.replace('%DATA%', '"%s"' % self.machine.settings['program_data'])
        with open(user_machine_file, 'w') as f: f.write(tmp_data)

        p_cmdline = [veriwell_bin, '-l', self.logfile, '-k', os.devnull, user_machine_file]

        try:
            process = Popen(p_cmdline, stdout=PIPE)
            self.data = process.communicate()[0]
            process.wait()
        except OSError:
            self.logger.die('error executing simulator')

        if process.returncode != 0:
            self.logger.die('simulator returned errorcode %s' %
                            process.returncode)

        self.check_and_split_sim_output()

    def check_and_split_sim_output(self):
        """
        Check sanity of the simulator's output and splits it accordingly.
        """
        s_ind = '=== start ==='
        e_ind = '=== end ==='

        if not (s_ind in self.data):
            self.logger.die('start indicator not found in sim output')

        if not (e_ind in self.data):
            self.logger.die('end indicator not found in sim output')

        if self.data.index(e_ind) < self.data.index(s_ind):
            self.logger.die('end indicator found before start indicator in sim output')

        self.data = self.data.split(s_ind)[1].split(e_ind)[0].strip()

    def run_standalone_simulation(self):
        """
        This functions runs a standalone simulation binary, pass the user's program/data
        files as command line arguments.
        """
        simulator_bin = self.find_simulator_binary()
        self.logger.info('running standalone simulation')
        p_cmdline = [simulator_bin, '--input', self.machine.settings['program_prog']]
        if not self.machine.single_mem:
            p_cmdline += [self.machine.settings['program_data']]

        try:
            process = Popen(p_cmdline, stdout=PIPE, stderr=PIPE)
            self.data, self.error = process.communicate()
            process.wait()
        except OSError:
            self.logger.die('error executing simulator')

        if self.data.strip() != '':
            self.logger.text('=== simulator stdout ===')
            self.logger.text(self.data)
            self.logger.text('=== end simulator stdout ===')

        if self.error.strip() != '':
            self.logger.error('=== simulator stderr ===')
            self.logger.error(self.error)
            self.logger.error('=== end simulator stderr ===')

        if process.returncode != 0:
            self.logger.die('simulator returned errorcode %s' %
                            process.returncode)

        self.check_and_split_sim_output()

    def run(self):
        """
        This is the main flowchart-like logic to deal with possible cases
        of missing machine file and/or simulator binary. Described in
        docs/simulation.dia.
        """
        machine = self.machine.machine
        simulator = self.machine.simulator

        if machine == '':
            self.logger.info('sim machine is empty, checking simulator')

            if simulator == '':
                self.logger.info('simulator empty, dying')
                self.logger.die('no simulator nor machine provided')
            else:
                self.logger.info('simulator not empty, checking')

                if simulator == 'veriwell':
                    self.logger.die('no machine given for verilog simulation')
                elif simulator == 'vhdl': # put other simulators here
                    self.logger.die('no machine given for vhdl simulation')
                else:
                    self.logger.info('sim type: standalone')
                    self.run_standalone_simulation()
        else:
            self.logger.info('sim machine not empty, checking simulator')

            if simulator == '':
                self.logger.info('simulator empty, checking extension')

                if machine.endswith('.v'):
                    self.logger.info('machine ends in .v, using veriwell')
                    self.run_veriwell_simulation()
                elif machine.endswith('.vh'): # put other simulators here
                    self.logger.info('machine ends in .vh, using vhdl')
                    # run vhdl simulation
                else:
                    self.logger.info('no simulator and unknown extension, dying')
                    self.logger.die('cannot determine simulator from extension')
            else:
                self.logger.info('simulator not empty, checking')

                if simulator == 'veriwell':
                    self.logger.info('machine not empty, simulator is veriwell')
                    self.run_veriwell_simulation()
                elif simulator == 'vhdl': # put other simulators here
                    self.logger.info('machine not empty, simulator is vhdl')
                else:
                    self.logger.info('unknown simulator, dying')
                    self.logger.die('unknown simulator given')
