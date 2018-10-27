#!/usr/bin/env python3

import logging as log

from os import environ as env
from os import path

import modules.extra as e

from modules.assembler  import Assembler
from modules.emulator   import Emulator
from modules.export     import export
from modules.settings   import Settings
from modules.simulation import Simulation

if __name__ == '__main__':
    fullFilename = path.dirname(__file__)
    env['PATH'] += ':' + path.join(fullFilename, 'bin')

    fmt = '%(levelname)s-%(module)s:%(lineno)s: %(message)s'
    log.basicConfig(format=fmt, level=log.NOTSET)

    e.settings = Settings()
    machine    = e.readJSONFile(e.settings.machineFile)
    assembler  = Assembler(machine)
    simulation = Simulation(machine)
    emulator   = Emulator(machine, simulation.data)

    if not emulator.advance(e.settings.step):
        log.error('emulator premature finish')
        exit(2)

    export(machine)
    e.settings.cleanup()
    exit(0)
