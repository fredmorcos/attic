"""
Main module of the Computer Architecture Simulator tool
"""
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
    FULL_FILENAME = path.dirname(__file__)
    env['PATH'] += ':' + path.join(FULL_FILENAME, 'bin')

    FMT = '%(levelname)s-%(module)s:%(lineno)s: %(message)s'
    log.basicConfig(format=FMT, level=log.NOTSET)

    e.settings = Settings()
    MACHINE    = e.readJSONFile(e.settings.machineFile)
    ASSEMBLER  = Assembler(MACHINE)
    SIMULATION = Simulation(MACHINE)
    EMULATOR   = Emulator(MACHINE, SIMULATION.data)

    if not EMULATOR.advance(e.settings.step):
        log.error('emulator premature finish')
        exit(2)

    export(MACHINE)
    e.settings.cleanup()
    exit(0)
