import os, sys
from exceptions import OSError
from subprocess import Popen, PIPE

def assemble_program(assembler_bin, filename):
    prog_fn = os.tempnam()
    data_fn = os.tempnam()
    lit_prog_fn = os.tempnam()
    lit_data_fn = os.tempnam()

    p_cmdline = [assembler_bin, filename, prog_fn, data_fn, lit_prog_fn, lit_data_fn]
    try:
        p = Popen(p_cmdline, stdout=PIPE)
        p.wait()
    except OSError:
        sys.stderr.write('Cannot find assembler binary: %s' % assembler_bin)
        exit(1)

    if p.returncode != 0:
        sys.stderr.write('Assembler %s returned with errorcode %s' % (assembler_bin, p.returncode))
        exit(1)

    res = {}
    res['__inst_mem_file'] = prog_fn
    res['__data_mem_file'] = data_fn
    res['__lit_inst_mem_file'] = lit_prog_fn
    res['__lit_data_mem_file'] = lit_data_fn
    with open(prog_fn, 'r') as f: res['__inst_mem'] = f.read()
    with open(data_fn, 'r') as f: res['__data_mem'] = f.read()
    with open(lit_prog_fn, 'r') as f: res['__lit_inst_mem'] = f.read()
    with open(lit_data_fn, 'r') as f: res['__lit_data_mem'] = f.read()

    return res
