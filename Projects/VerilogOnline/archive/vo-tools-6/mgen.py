#!/usr/bin/python2

import os, warnings, json
from sys import stdout, stderr
from argparse import ArgumentParser
from exceptions import OSError
from ConfigParser import ConfigParser
from machinegen.logger import Logger
from machinegen.machine import Machine
from machinegen.assembler import Assembler
from machinegen.simulation import Simulation
from machinegen.emulator import Emulator
from machinegen.json_output import JSONOutput

SETTINGS = {}

def read_settings():
    argparser = ArgumentParser(description='Online Machine Simulation Tool')
    argparser.add_argument('--machine', type=str, required=True, metavar='M',
                           help='Machine name M to run')
    argparser.add_argument('--step', type=int, required=True, metavar='S',
                           help='Step number to retrieve'),
    argparser.add_argument('--config', default='config/general.conf', type=str, metavar='FILE',
                           help='Config file to load [default: %(default)s]')
    argparser.add_argument('--text', default=stdout.fileno(), type=int, metavar='FD',
                           help='Text output file descriptor [default: stdout]')
    argparser.add_argument('--error', default=stderr.fileno(), type=int, metavar='FD',
                           help='Error output file descriptor [default: stderr]')
    argparser.add_argument('--warn', default=stderr.fileno(), type=int, metavar='FD',
                           help='Warning output file descriptor [default: stderr]')
    argparser.add_argument('--result', default=7, type=int, metavar='FD',
                           help='Result output file descriptor [default: %(default)s]')
    argparser.add_argument('--sim-log', default=os.devnull, type=str, metavar='FILE',
                           help='Dump simulation log to FILE [default: %(default)s]')
    argparser.add_argument('files', metavar='FILES', nargs='+',
                           help='Assembly/machine program files to compile and run')
    args = argparser.parse_args()

    configparser = ConfigParser()
    configparser.read(args.config)

    SETTINGS['machine']     = args.machine
    SETTINGS['step']        = args.step
    SETTINGS['config']      = args.config
    SETTINGS['text']        = os.fdopen(args.text, 'w')
    SETTINGS['error']       = os.fdopen(args.error, 'w')
    SETTINGS['warn']        = os.fdopen(args.warn, 'w')
    SETTINGS['files']       = args.files

    try:
        SETTINGS['result']  = os.fdopen(args.result, 'w')
    except OSError:
        SETTINGS['result']  = SETTINGS['text']

    SETTINGS['sim_log']     = args.sim_log

    SETTINGS['assemblers']  = configparser.get('directories', 'assemblers')
    SETTINGS['simulators']  = configparser.get('directories', 'simulators')
    
    SETTINGS['machines']    = configparser.get('directories', 'machines')
    SETTINGS['definitions'] = configparser.get('directories', 'definitions')
    SETTINGS['templates']   = configparser.get('directories', 'templates')
    SETTINGS['images']      = configparser.get('directories', 'images')

    SETTINGS['def_ext']     = '.' + configparser.get('extensions', 'def_ext')

    SETTINGS['logger']      = Logger(SETTINGS)

def cleanup():
    if SETTINGS.has_key('program_prog'):      os.remove(SETTINGS['program_prog'])
    if SETTINGS.has_key('program_data'):      os.remove(SETTINGS['program_data'])
    if SETTINGS.has_key('program_dprog'):     os.remove(SETTINGS['program_dprog'])
    if SETTINGS.has_key('program_ddata'):     os.remove(SETTINGS['program_ddata'])
    if SETTINGS.has_key('user_machine_file'): os.remove(SETTINGS['user_machine_file'])

if __name__ == '__main__':
    warnings.simplefilter('ignore')
    read_settings()

    logger = SETTINGS['logger']

    if SETTINGS['step'] < 0:
        logger.die('step should be >= 0')

    machine = Machine(SETTINGS['machine'], SETTINGS)
    assembler = Assembler(machine)
    assembler.run()
    simulation = Simulation(machine)
    simulation.run()
    emulator = Emulator(machine, simulation.data)
    res = emulator.advance(SETTINGS['step'])

    if not res:
        logger.error('traverser premature finish')
        exit(2)

    out = JSONOutput(machine)
    res = out.export()
    json.dump(res, SETTINGS['result'])

    cleanup()
    exit(0)
