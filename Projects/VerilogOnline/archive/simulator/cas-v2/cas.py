#!/usr/bin/env python3
""" main module for comp. architecture simulator """

import json
import logging as log

from os import fdopen
from os import devnull
from os.path import abspath
from tempfile import mkstemp
from subprocess import PIPE
from subprocess import Popen

from argparse import ArgumentParser as ArgPar

class Configuration:
    """ carries configuration throughout the program """
    def __init__(self, args):
        self.machine_file = abspath(args.machine_file)
        self.program_file = abspath(args.program_file)
        self.data_file    = abspath(args.data_file)
        self.simlog       = abspath(args.simlog)
        self.step         = args.step
        self.cycles       = args.cycles

        self.machine_data = None

        self.check()
        self.load_data()
        self.log()

    def check(self):
        """ checks sanity of configuration """
        if self.step < 0:
            die("step must be >= 0")

    def load_data(self):
        """ loads machine json data """
        with open(self.machine_file, "r") as mfile:
            self.machine_data = json.load(mfile)

    def log(self):
        """ send to log output some info """
        log.info("machine: %s", self.machine_file)
        log.info("   step: %s", self.step)
        log.info("program: %s", self.program_file)
        log.info("   data: %s", self.data_file)
        log.info(" simlog: %s", self.simlog)

def die(msg):
    """ prints critical log and exits with error """
    log.critical(msg)
    exit(1)

def parse_cmdline_args():
    """ parses cmdline arguments and returns a configuration """
    argpar = ArgPar(description="Online Machine Simulation Tool")
    addarg = argpar.add_argument

    addarg("machine_file", type = str, metavar = "MACHINE_FILE",
           help = "Machine filename to simulate")
    addarg("program_file", metavar = "PROG_FILE",
           help = "Program file to run")
    addarg("data_file", metavar = "DATA_FILE",
           help = "Data file to load")
    addarg("step", type = int, metavar = "STEP",
           help = "Step number to retrieve")
    addarg("cycles", type = int, metavar = "CYCLES",
           help = "Number of simulation cycles")
    addarg("-x", "--simlog", default = devnull,
           type = str, metavar = "LF",
           help = "Dump simulation log to file LF [%(default)s]")

    return Configuration(argpar.parse_args())

def run_sim(conf):
    """ runs (veriwell) simulation and returns output """
    machine = conf.machine_data
    simcode = machine["simcode"]

    # replace steps, program file and data file
    # in (veriwell) simulator code
    simcode = simcode \
        .replace("%CYCLES%", str(conf.cycles)) \
        .replace("%PROG%", "\"%s\"" % conf.program_file) \
        .replace("%DATA%", "\"%s\"" % conf.data_file)

    # write machine-specific simcode to file and run it
    simcode_file, simcode_fn = mkstemp(prefix="ca_sim_",
                                       suffix="_simcode",
                                       text=True)
    log.info("simcode: %s", simcode_fn)

    with fdopen(simcode_file, "w") as simcodef:
        simcodef.write(simcode)

    # run simulator
    cmd = ["veriwell", "-l", conf.simlog, "-k", devnull, simcode_fn]

    # execute (veriwell) simulator and parse output
    with Popen(cmd, stdout=PIPE, stderr=PIPE) as proc:
        out = proc.stdout.read().decode()
        err = proc.stderr.read().decode()

    if err:
        log.critical(err)
        exit(1)

    if proc.returncode != 0:
        log.critical("veriwell returned %s", str(proc.returncode))
        exit(1)

    return out                       \
      .partition("=== start ===")[2] \
      .partition("=== end ===")[0]   \
      .strip()

def prepare_emulation_data(conf, sim_data):
    """ cleans up and prepares simulation data for emulation """
    data = [i.strip() for i in sim_data.splitlines()]   # strip lines
    data = [i.split(":") for i in data]
    return [[i[0], i[1], i[2].split(",") if i[0] != 'mw' else None]
            for i in data]                # split state elem data

def emulation(conf, sim_data):
    """ run clock cycle emulation and updates machine data """
    if conf.step == 0:
        pass
    i = row = 0

def main():
    """ entrance function """
    log_format = "%(levelname)s:%(module)s:%(lineno)s: %(message)s"
    log.basicConfig(format=log_format, level=log.NOTSET)

    conf = parse_cmdline_args()
    data = run_sim(conf)
    data = prepare_emulation_data(conf, data)
    emulation(conf, data)

    # TODO
    # advance emulation
    # update machine data
    # export machine to json

if __name__ == "__main__":
    main()
