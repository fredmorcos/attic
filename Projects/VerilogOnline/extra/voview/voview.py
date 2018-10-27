#!/usr/bin/env python3

import logging
import tkinter as tk
from argparse import ArgumentParser
from app import App

if __name__ == '__main__':
    logging.basicConfig(
        format='%(levelname)s-%(module)s.%(funcName)s:%(lineno)s: %(message)s',
        level=logging.NOTSET)

    ap = ArgumentParser(description='UI and Viewer for Machine Simulation Tool')
    ap.add_argument('machine', type=str, metavar='MACHINE',
                    help='Machine name M to run')
    ap.add_argument('files', metavar='FILES', nargs='+',
                    help='Assembly/machine program files to compile and run')
    ap.add_argument('-d', '--mach-dir', default='machines/', type=str, metavar='D',
                    help='Directory with program files [%(default)s]')
    args = ap.parse_args()

    root = tk.Tk()
    app = App(root, machine=args.machine, files=args.files, mach_dir=args.mach_dir)
    root.mainloop()
