import os, shutil
from os import path
from subprocess import Popen, PIPE
from exceptions import OSError

class Assembler:
    """
    Takes care of all special cases with single and double mems, support
    for debug info or not, etc... and executes the assembler accordingly
    or just loads the programs. If reaches a point to no avail, fails with
    an error.
    """
    def __init__(self, machine):
        """
        Machine is the machine to assemble the program for.
        """
        self.machine = machine

    def try_assembling(self, program, die_on_fail=True):
        settings = self.machine.settings
        logger = settings['logger']
        
        assembler_bin = path.join(self.machine.settings['assemblers'],
                                  self.machine.assembler)
        p_cmdline = [assembler_bin, program]

        prog = os.tempnam()
        p_cmdline += [prog]
        settings['program_prog'] = prog

        if not self.machine.single_mem:
            data = os.tempnam()
            p_cmdline += [data]
            settings['program_data'] = data

        if self.machine.debug_info:
            dprog = os.tempnam()
            p_cmdline += [dprog]
            settings['program_dprog'] = dprog

            if not self.machine.single_mem:
                ddata = os.tempnam()
                p_cmdline += [ddata]
                settings['program_ddata'] = ddata

        logger.info('assembler cmdline %s' % p_cmdline)

        try:
            process = Popen(p_cmdline, stdout=PIPE, stderr=PIPE)
            stdout_data, stderr_data = process.communicate()
            process.wait()
        except OSError:
            logger.die('error executing assembler')

        if stdout_data.strip() != '':
            logger.text('=== assembler stdout ===')
            logger.text(stdout_data)
            logger.text('=== end assembler stdout ===')

        if stderr_data.strip() != '':
            logger.error('=== assembler stderr ===')
            logger.error(stderr_data)
            logger.error('=== end assembler stderr ===')

        if die_on_fail:
            if process.returncode != 0:
                logger.die('assembler returned errorcode %s' %
                           process.returncode)
        return process.returncode

    def find_data_prog_files(self, files):
        if files[0].endswith('.d'):
            data_file = files[0]
        elif files[1].endswith('.d'):
            data_file = files[1]
        else:
            logger.die('cannot find data (.d) file')

        if files[0].endswith('.p'):
            program_file = files[0]
        elif files[1].endswith('.p'):
            program_file = files[1]
        else:
            logger.die('cannot find program (.p) file')

        return data_file, program_file

    def run(self):
        """
        Runs assembler according to flowchart in docs/assembler.dia
        The old flowchart (in docs/assembler-old.dia) code is
        commented out below.
        """
        settings = self.machine.settings
        logger = settings['logger']
        files = settings['files']
        assembler = self.machine.assembler

        if len(files) == 1:
            logger.info('single file provided')
            
            if self.machine.single_mem:
                logger.info('machine is single memory')

                if assembler == '':
                    logger.info('no assembler, using input directly')
                    tmp = os.tempnam()
                    shutil.copyfile(files[0], tmp)
                    settings['program_prog'] = tmp
                    self.machine.debug_info = False
                else:
                    logger.info('have assembler, trying to assemble')
                    
                    returncode = self.try_assembling(files[0], die_on_fail=False)
                    logger.info('assembler returned errorcode %s' %
                                returncode)
                    
                    if returncode != 0:
                        logger.info('assembler failed, using input directly')
                        tmp = os.tempnam()
                        shutil.copyfile(files[0], tmp)
                        settings['program_prog'] = tmp
                        self.machine.debug_info = False
                    else:
                        logger.info('assembler succeeded')
            else:
                logger.info('machine is dual memory')
                
                if assembler == '':
                    logger.die('machine does not have an assembler')
                else:
                    logger.info('machine has an assembler')
                    self.try_assembling(files[0])
        elif len(files) == 2:
            logger.info('two files provided')

            if self.machine.single_mem:
                logger.info('machine is single mem, merging files (data first)')

                data_file, prog_file = self.find_data_prog_files(files)

                with open(data_file, 'r') as f: data  = f.read()
                with open(prog_file, 'r') as f: data += f.read()

                name = os.tempnam()
                with open(name, 'r') as f: f.write(data)
                settings['program_prog'] = name

                self.machine.debug_info = False
            else:
                logger.info('machine is dual mem')
                
                data_file, prog_file = self.find_data_prog_files(files)

                tmp = os.tempnam()
                shutil.copyfile(prog_file, tmp)
                settings['program_prog'] = tmp

                tmp = os.tempnam()
                shutil.copyfile(data_file, tmp)
                settings['program_data'] = tmp

                self.machine.debug_info = False
        else:
            logger.die('more than 2 files given')

        # if ',' in program:
        #     logger.info('multi-file program')

        #     if program.count(',') > 1:
        #         logger.info('more than 2 program files, dying')
        #         logger.die('got more than 2 program files')
        #     else:
        #         logger.info('exactly 2 program files, checking')
        #         prog, data = program.split(',')

        #         if prog.endswith('.p'):
        #             logger.info('program is fine, ends in .p')

        #             if data.endswith('.d'):
        #                 logger.info('data is fine, ends in .d')

        #                 if self.machine.single_mem:
        #                     logger.info('single mem machine, combining')

        #                     tmp_fn = os.tempnam()
        #                     with open(tmp_fn, 'w') as f:
        #                         tempdata = ''
        #                         with open(prog, 'r') as f: tempdata += f.read() + '\n'
        #                         with open(data, 'r') as f: tempdata += f.read()
        #                         f.write(tempdata)
        #                     self.machine.settings['program_prog'] = tmp_fn
        #                 else:
        #                     logger.info('dual mem machine, loading')

        #                     tmp_fn = os.tempnam()
        #                     shutil.copyfile(prog, tmp_fn)
        #                     self.machine.settings['program_prog'] = tmp_fn
        #                     tmp_fn = os.tempnam()
        #                     shutil.copyfile(data, tmp_fn)
        #                     self.machine.settings['program_data'] = tmp_fn
        #             else:
        #                 logger.info('data does not end in .d, dying')
        #                 logger.die('invalid data file extension')
        #         else:
        #             logger.info('program does not end in .p, dying')
        #             logger.die('invalid program file extension')
        # else:
        #     logger.info('single-file program')

        #     if program.endswith('.s'):
        #         logger.info('got an assembly program, checking assembler')

        #         if assembler == '':
        #             logger.info('no assembler, dying')
        #             logger.die('given assembly program, but no assembler for machine')
        #         else:
        #             logger.info('have an assembler, assemble')

        #             assembler_bin = path.join(self.machine.settings['assemblers'],
        #                                       self.machine.assembler)
        #             p_cmdline = [assembler_bin, program]

        #             prog = os.tempnam()
        #             p_cmdline += [prog]
        #             self.machine.settings['program_prog'] = prog

        #             if not self.machine.single_mem:
        #                 data = os.tempnam()
        #                 p_cmdline += [data]
        #                 self.machine.settings['program_data'] = data

        #             if self.machine.debug_info:
        #                 dprog = os.tempnam()
        #                 p_cmdline += [dprog]
        #                 self.machine.settings['program_dprog'] = dprog

        #                 if not self.machine.single_mem:
        #                     ddata = os.tempnam()
        #                     p_cmdline += [ddata]
        #                     self.machine.settings['program_ddata'] = ddata

        #             logger.info('assembler cmdline %s' % p_cmdline)

        #             try:
        #                 process = Popen(p_cmdline, stdout=PIPE, stderr=PIPE)
        #                 stdout_data, stderr_data = process.communicate()
        #                 process.wait()
        #             except OSError:
        #                 logger.die('error executing assembler')

        #             if stdout_data.strip() != '':
        #                 logger.text('=== assembler stdout ===')
        #                 logger.text(stdout_data)
        #                 logger.text('=== end assembler stdout ===')

        #             if stderr_data.strip() != '':
        #                 logger.error('=== assembler stderr ===')
        #                 logger.error(stderr_data)
        #                 logger.error('=== end assembler stderr ===')

        #             if process.returncode != 0:
        #                 logger.die('assembler returned errorcode %s' %
        #                            process.returncode)
        #     else:
        #         logger.info('not an assembly program, checking machine')

        #         if self.machine.single_mem:
        #             logger.info('machine is single mem, checking program')

        #             if program.endswith('.p'):
        #                 logger.info('program ends in .p, loading')

        #                 tmp_fn = os.tempnam()
        #                 shutil.copyfile(program, tmp_fn)
        #                 self.machine.settings['program_prog'] = tmp_fn
        #             else:
        #                 logger.info('program does not end in .p, dying')
        #                 logger.die('program does not comply with machine')
        #         else:
        #             logger.info('machine is double mem, dying')
        #             logger.die('program does not comply with machine')
