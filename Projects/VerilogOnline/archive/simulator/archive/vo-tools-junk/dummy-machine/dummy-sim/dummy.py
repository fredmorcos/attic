#!/usr/bin/env python3

from os import access, R_OK
from sys import stderr
from argparse import ArgumentParser

if __name__ == '__main__':
    ap = ArgumentParser(description='Simulates a dummy machine')
    ap.add_argument('--input', required=False, default='%PROG%',
                    help='Optional input file')
    args = ap.parse_args()

    if not access(args.input, R_OK):
        stderr.write('cannot open file %s\n' % args.input)
        exit(1)

    with open(args.input, 'r') as f:
        dmem = f.readlines()
        for i, e in enumerate(dmem):
            dmem[i] = int(e)

    time = 0
    clk  = 0
    val1 = 0
    val2 = 0
    inc  = 'INC'
    addr = 0

    print(dmem)
    print('=== start ===')
    while time < 200:
        print('%(time)s:%(clk)s:%(val1)s,%(val2)s,%(inc)s,%(addr)s' % locals())
        # TODO add memory operations output

        if clk == 1:
            val1       = dmem[addr]
            val2       = val1 + 1
            dmem[addr] = val2
            addr += 1

        clk   = int(not bool(clk))
        time += 10
    print('=== end ===')
    print(dmem)
