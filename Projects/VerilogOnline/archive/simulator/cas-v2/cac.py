#!/usr/bin/env python3
""" compiles a machine definition with its image and simulation code """

import json

from sys import stdin
from sys import stderr

def die(msg):
    log(msg)
    exit(1)

def log(msg):
    stderr.write(msg + '\n')

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

def load_machine(machine, toplevel=False):
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

def main():
    machine_data = json.load(stdin)
    machine = load_machine(machine_data, toplevel=True)
    print(json.dumps(machine, sort_keys=True, indent=2))

if __name__ == '__main__':
    main()
