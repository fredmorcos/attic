#!/usr/bin/env python
from sys import stderr

from os import access
from os import R_OK as READ

from argparse import ArgumentParser as ArgParser

from json import load as _json_load
from json import dumps as json_dumps

def die(msg): log(msg); exit(1);
def log(msg): stderr.write(msg + '\n');

def can_read_file(f):
    if not access(f, READ): die('cannot open %s for reading' % f)

def read_file(filename):
    can_read_file(filename)
    with open(filename, 'r') as f: res = f.read()
    return res

def json_load(filename):
    can_read_file(filename)
    with open(filename, 'r') as f: res = _json_load(f)
    return res

def check_machine(m):
    # do some checks and return False if m is a memory and True otherwise
    if 'title' not in m: die('machine has no title')
    if 'mem_name'  in m: log('loaded mem %s' % m['title']); return False;
    if 'image' not in m: die('machine (%s) has no image' % m['title'])
    return True

def load_toplevel_machine(m):
    m['single_mem'] = m.get('single_mem', True)
    m['debug_info'] = m.get('debug_info', False)
    m['assembler']  = m.get('assembler', None)
    m['simcode']    = m.get('simcode',   None)
    m['simulator']  = m.get('simulator', None)

def load_machine(m, toplevel=False):
    if not check_machine(m): return m # it's a memory element
    if toplevel:
        load_toplevel_machine(m)
        if m['simcode']: m['simcode'] = read_file(m['simcode'])

    m['image'] = json_load(m['image'])
    m['image']['title'] = m['title']

    for k,v in m.get('elements', {}).items():
        m['elements'][k] = {'index': v[0], 'can_toggle': v[1],
                            'value': None, 'changed': False}

    for k,v in m.get('sub_elements', {}).items():
        if   isinstance(v, dict): tmp_data = v
        elif isinstance(v, str):  tmp_data = json_load(v)
        else: die('unsupported submachine definition for %s' % k)
        m['sub_elements'][k] = load_machine(tmp_data)

    log('loaded machine %s' % m['title'])
    return m

if __name__ == '__main__':
    ap = ArgParser(description="VO Format Compiler")
    addarg = ap.add_argument
    addarg('machinedef', type=str, metavar='F', help='Machine def file')
    args = ap.parse_args()

    json_data = json_load(args.machinedef)
    machine = load_machine(json_data, toplevel=True)
    print(json_dumps(machine, sort_keys=True, indent=2))
