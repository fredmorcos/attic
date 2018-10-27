#!/usr/bin/env python

import datetime
import json
import sys

from argparse import ArgumentParser as AP
from os import path

printTask = lambda t: print("%(id)s %(done) %(date)s %(text)s" % t)


def readFile(filename):
    try:
        file = open(filename)
    except IOError as e:
        sys.stderr.write(str(e) + "\n")
        sys.stderr.write("Try 'touch " + filename + "'\n")
        exit(1)
    else:
        try:
            return json.load(file)
        except:
            return []


if __name__ == "__main__":
    parser = AP(description="Task/Todo List Manager")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--list",
                       help="List all task items",
                       action="store_true")
    group.add_argument("--add",
                       help="Add an item",
                       type=str,
                       dest="newItem",
                       metavar="DESC")
    group.add_argument("--del",
                       help="Delete an item",
                       type=int,
                       dest="delItem",
                       metavar="NUM")
    group.add_argument("--done",
                       help="Set an item as 'done'",
                       type=int,
                       dest="doneItem",
                       metavar="NUM")
    args = parser.parse_args()

    userfile = path.expanduser("~/.tasker")
    taskList = readFile(userfile)

    if args.list:
        for task in taskList:
            printTask(task)
    elif args.newItem:
        pass
