#!/usr/bin/python2

import sys, argparse, os, warnings
from ConfigParser import ConfigParser
from libm2g.graph import Graph
from libm2g.assembler import assemble_program
from libm2g.simulation import run_verilog_sim
from libm2g.traverser import Traverser, TraverserError, TraverserFinished

def __prepare_args():
    parser = argparse.ArgumentParser(description='Convert a Verilog simulation to an architecture diagram')
    parser.add_argument('config', help='Configuration file')
    parser.add_argument('machine', help='Machine name')
    parser.add_argument('step_num', help='The requested step number in the simulation')
    parser.add_argument('program', help='Program filename to execute on the machine')
    parser.add_argument('--dump-verilog', dest='dump_verilog', action='store_const',
                        const=True, default=False, help='Dump Verilog output to verilog.log')
    return parser.parse_args()

if __name__ == '__main__':
    warnings.simplefilter('ignore')
    args = __prepare_args()

    cp = ConfigParser()
    cp.read(args.config)
    machines_dir = cp.get('directories', 'machines')
    web_dir      = cp.get('directories', 'web')
    bin_dir      = cp.get('directories', 'bin')
    
    machine_def_file = os.path.join(machines_dir, args.machine, args.machine + '.json')
    graph = Graph(machine_def_file)
    assembler_bin_file = os.path.join(bin_dir, graph.assembler)
    page_template_file = os.path.join(web_dir, 'page.html')

    prog_data = assemble_program(assembler_bin_file, args.program)

    with open(graph.verilog_file, 'r') as f: verilog_data = f.read()
    user_verilog_data = verilog_data.replace('%PROG%', '"%s"' % prog_data['__inst_mem_file']). \
                                     replace('%DATA%', '"%s"' % prog_data['__data_mem_file'])
    user_verilog_file = os.tempnam()
    with open(user_verilog_file, 'w') as f: f.write(user_verilog_data)

    traverser = Traverser(run_verilog_sim(user_verilog_file, args.dump_verilog), graph)
    os.remove(user_verilog_file)
    os.remove(prog_data['__inst_mem_file'])
    os.remove(prog_data['__data_mem_file'])
    os.remove(prog_data['__lit_inst_mem_file'])
    os.remove(prog_data['__lit_data_mem_file'])
    
    try:
        traverser.first_step()
        traverser.update_registered()
        traverser.graph.unmark_all()
    except TraverserError:
        exit(1)
    except TraverserFinished:
        exit(2)

    steps = int(args.step_num)
    exit_code = 0

    if steps < 0:
        exit(1)
    elif steps == 0:
        pass
    else:
        while steps > 0:
            try:
                exit_code = traverser.advance()
                if exit_code == 2:
                    break
                else:
                    exit_code = 0
            except TraverserFinished:
                exit(2)
                
            traverser.update_registered()
            if steps != 1:
                traverser.graph.unmark_all()
            steps -= 1
        traverser.update_registered()

    print(graph.export(page_template_file, prog_data))
    exit(exit_code)
