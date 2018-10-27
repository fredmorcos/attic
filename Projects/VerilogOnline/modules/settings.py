import logging, sys
from argparse import ArgumentParser as ArgParser
from os import devnull, fdopen, path, remove
from modules.extra import die

class Settings:
    def __init__(self):
        ap = ArgParser(description='Online Machine Simulation Tool')

        addarg = ap.add_argument
        outf = sys.stdout.fileno()
        errf = sys.stderr.fileno()

        addarg('machine', type=str, metavar='MACHINE',
               help='Machine name M to run')
        addarg('step', type=int, metavar='STEP',
               help='Step number to retrieve')
        addarg('files', metavar='FILES', nargs='+',
               help='Program files to compile and run')
        addarg('-m', '--max-steps', default=500, type=int, metavar='N',
               help='Maximum number of steps [%(default)s]')
        addarg('-x', '--range', default=10, type=int, metavar='N',
               help='Range of steps to cache [%(default)s]')
        addarg('-f', '--format', default='js', type=str, metavar='FMT',
               help='Output format (js|json|jsc|jsonc) [%(default)s]')
        addarg('-d', '--mach-dir', default='machines/', type=str, metavar='D',
               help='Directory with machine files [%(default)s]')
        addarg('-t', '--text', default=outf, type=int, metavar='FD',
               help='Text output file descriptor [stdout]')
        addarg('-e', '--error', default=errf, type=int, metavar='FD',
               help='Error output file descriptor [stderr]')
        addarg('-w', '--warn', default=errf, type=int, metavar='FD',
               help='Warning output file descriptor [stderr]')
        addarg('-r', '--result', default=7, type=int, metavar='FD',
               help='Result output file descriptor [%(default)s]')
        addarg('-l', '--sim-log', default=devnull, type=str, metavar='F',
               help='Dump simulation log to file [%(default)s]')
        args = ap.parse_args()

        # do some sanity checks
        if args.range > args.max_steps: die('bad range > max_steps')
        if args.step < 0:               die('step should be >= 0')

        self.machineName = args.machine
        self.step        = args.step
        self.machineFile = path.join(args.mach_dir, args.machine, 'main.machine')
        self.text        = fdopen(args.text, 'w')
        self.error       = fdopen(args.error, 'w')
        self.warn        = fdopen(args.warn, 'w')
        self.files       = args.files
        self.simLogFile  = args.sim_log
        self.format      = args.format
        self.maxSteps    = args.max_steps
        self.range       = args.range

        try:            self.result = fdopen(args.result, 'w')
        except OSError: self.result = self.text

    def cleanup(self):
        d = self.__dict__
        f = ['program_prog',      'program_data',
             'program_dprog',     'program_ddata',
             'user_machine_file', 'simcode_file']
        [remove(d[i]) for i in f if i in d]

        self.text.flush()
        self.error.flush()
        self.warn.flush()
        self.result.flush()
