import sys, os
from exceptions import OSError
from subprocess import Popen, PIPE

def run_verilog_sim(machine, dump=False):
    if dump == False:
        import os
        verilog_out = os.devnull
    else:
        verilog_out = 'verilog.log'

    p_cmdline = ['veriwell', '-l', verilog_out, '-k', os.devnull, machine]
    try:
        p = Popen(p_cmdline, stdout=PIPE)
        sim_data = p.communicate()[0].split('=== start ===')[1].split('=== end ===')[0].strip()
        p.wait()
    except OSError:
        sys.stderr.write('Cannot find simulation binary: veriwell')
        exit(1)

    if p.returncode != 0:
        sys.stderr.write('Simulation returned with errorcode %s' % p.returncode)
        exit(1)
    
    return sim_data
