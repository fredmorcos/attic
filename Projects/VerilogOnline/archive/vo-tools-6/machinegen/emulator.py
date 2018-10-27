class Emulator:
    """
    This class is used to parse simulation output to find a certain posedge
    clock cycle. In other words, a clock cycle emulator.
    """
    def __init__(self, machine, sim_data):
        """
        Machine is the simulation's machine.
        SimData cannot be empty.
        """
        if sim_data.strip() == '':
            self.logger.die('empty sim data')

        self.machine = machine
        self.data = sim_data.splitlines()
        self.logger = machine.settings['logger']

    def parse_line(self, row=0):
        """
        Returns splitted line at row.
        """
        return self.data[row].strip().split(':')

    def check_cycle_line(self, line):
        """
        Some sanity checks on split line (that time and clk are integers).
        """
        try:    time = int(line[0])
        except: self.logger.die('first col (time) in sim output not int')
        try:    clk = int(line[1], 2)
        except: self.logger.die('second col (clk) in sim output not bin')
        self.logger.info('emulator pass, time=%s, clk=%s' % (time, clk))
        return time, clk

    def advance(self, step=0):
        """
        Moves through the simulation data, parsing every line. Works
        according to the emulator state machine:

        START: Get 0 -> S0
        S0:    Get 0 -> S0
        S0:    Get 1 -> S1
        S1:    Get 1 -> S1
        S1:    Get 0 -> S0, step++, machine.load_values()
        END when counter == wanted_steps or premature_finish

        Returns True for success
        Returns False for premature finish
        """
        if len(self.data) == 0:
            self.logger.die('empty sim data')

        if step < 0:
            self.logger.die('bad step number %s' % step)

        if step == 0:
            tmp_line = self.parse_line()
            if tmp_line[0] == 'mw':
                self.logger.die('step 0 is a memwrite operation')
            self.check_cycle_line(tmp_line)
            self.machine.load_mem()
            self.machine.load_values(tmp_line[2].split(','))
            self.machine.unmark_all()
        else:
            i = 0
            row = 0
            state = 0

            self.machine.load_mem()

            while i < step:
                if row == len(self.data) - 1:
                    return False

                self.machine.unmark_all()
                tmp_line = self.parse_line(row)

                if tmp_line[0] == 'mw':
                    self.machine.mem_write(tmp_line[1].split(','))
                else:
                    time, clk = self.check_cycle_line(tmp_line)
                    if state == 0 and clk == 1:
                        if clk == 1:
                            state = 1
                    elif state == 1 and clk == 0:
                        if clk == 0:
                            state = 0
                            i += 1
                            prev_line = self.parse_line(row - 1)
                            self.logger.info('applying line at time=%s, clk=%s' %
                                             (prev_line[0], prev_line[1]))
                            self.machine.load_values(prev_line[2].split(','))
                row += 1
        return True
