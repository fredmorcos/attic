import shutil
import logging as log
import modules.extra as e

from modules.extra import die
from modules.extra import execProcess
from modules.extra import mktempf

class Assembler:
    # Takes care of all special cases with single and double mems, support
    # for debug info or not, etc... and executes the assembler accordingly
    # or just loads the programs. If reaches a point to no avail, fails with
    # an error.
    def __init__(self, machine):
        self.machine = machine
        self.run()

    def _tryAssembling(self, program, canDie=True):
        p_cmdline = [self.machine['assembler'], program]

        prog = mktempf('_prog')
        p_cmdline += [prog]
        e.settings.program_prog = prog

        if not self.machine['single_mem']:
            data = mktempf('_data')
            p_cmdline += [data]
            e.settings.program_data = data

        if self.machine['debug_info']:
            dprog = mktempf('_dprog')
            p_cmdline += [dprog]
            e.settings.program_dprog = dprog

            if not self.machine['single_mem']:
                ddata = mktempf('_ddata')
                p_cmdline += [ddata]
                e.settings.program_ddata = ddata

        # log.info('assembler cmdline %s' % p_cmdline)
        out, err, rc = execProcess(p_cmdline, 'assembler',
                                   showOut=False, canDie=canDie)
        return rc

    def _findDataProgFiles(self, files):
        if files[0].endswith('.d'):   data_file = files[0]
        elif files[1].endswith('.d'): data_file = files[1]
        else: die('cannot find data (.d) file')

        if files[0].endswith('.p'):   program_file = files[0]
        elif files[1].endswith('.p'): program_file = files[1]
        else: die('cannot find program (.p) file')

        return data_file, program_file

    def run(self):
        # Runs assembler according to flowchart in docs/assembler.dia
        files = e.settings.files
        assembler = self.machine['assembler']

        if len(files) == 1:
            log.info('single file provided')
            
            if self.machine['single_mem']:
                log.info('machine is single memory')

                if not assembler:
                    log.info('no assembler, using input directly')
                    tmp = mktempf('_tmp')
                    shutil.copyfile(files[0], tmp)
                    e.settings.program_prog = tmp
                    self.machine['debug_info'] = False
                else:
                    log.info('have assembler, trying to assemble')
                    
                    returncode = self._tryAssembling(files[0], canDie=False)
                    log.info('assembler returned errorcode %s' % returncode)
                    
                    if returncode != 0:
                        log.info('assembler failed, using input directly')
                        tmp = mktempf(suffix='_tmp')
                        shutil.copyfile(files[0], tmp)
                        e.settings.program_prog = tmp
                        self.machine['debug_info'] = False
                    else: log.info('assembler succeeded')
            else:
                log.info('machine is dual memory')
                
                if not assembler: die('machine does not have an assembler')
                else:
                    log.info('machine has an assembler')
                    self._tryAssembling(files[0])
        elif len(files) == 2:
            log.info('two files provided')

            if self.machine['single_mem']:
                log.info('machine is single mem, merging files (data first)')

                dataFile, progFile = self._findDataProgFiles(files)

                with open(dataFile, 'r') as f: data  = f.read()
                with open(progFile, 'r') as f: data += f.read()

                name = mktempf('_name')
                with open(name, 'w') as f: f.write(data)
                e.settings.program_prog = name

                self.machine['debug_info'] = False
            else:
                log.info('machine is dual mem')
                
                dataFile, progFile = self._findDataProgFiles(files)

                tmp = mktempf('_tmp')
                shutil.copyfile(progFile, tmp)
                e.settings.programProg = tmp

                tmp = mktempf('_tmp')
                shutil.copyfile(dataFile, tmp)
                e.settings.program_data = tmp

                self.machine['debug_info'] = False
        else: die('more than 2 files given')
