#!/usr/bin/env python3

"""
Follows the FIG 3.2 format specification.
"""

import extra
from argparse import ArgumentParser
from json import dump as json_dump
from extra import log_and_die
from polyline import parse_polyline
from text import parse_text
from sys import stderr, stdout

def skip_header(input_file): # skip first 7 lines and comments
    num = 0
    for line in input_file:
        line = line.strip()
        if line.startswith('#'):
            continue
        if num == 7:
            break
        num += 1

def parse_object(line, input_file):
    tmp_line = line.split(' ')

    if tmp_line[0] == '#':
        meta_name = tmp_line[1]
        for l in input_file:
            res = parse_object(l, input_file)
            # ignore if comment before compound type
            if len(res) > 1: pass
            else: res[0]['metaname'] = meta_name
            return res

    typ = int(tmp_line[0])

    if typ == 2: # polyline, polygon, box
        return [parse_polyline(tmp_line, input_file)]
    elif typ == 4: # text
        return [parse_text(tmp_line, input_file)]
    elif typ == 6: # compound object, we're not interested
        stderr.write('found a compound (group) object, try to avoid that\n')
        res = []
        for l in input_file:
            if l.strip() == '-6':
                break
            res += parse_object(l, input_file)
        return res
    else:
        log_and_die('type %s not supported' % typ)

if __name__ == '__main__':
    argparser = ArgumentParser(description='Convert a FIG file to JSON')
    argparser.add_argument('input', help='Input FIG filename to convert')
    args = argparser.parse_args()

    objects = []

    with open(args.input, 'r') as input_file:
        skip_header(input_file)

        for line in input_file:
            line = line.strip()

            if line.startswith('0'):
                pass # skip colors
            else:
                objects += parse_object(line, input_file)

    # get width and height of document
    first = True
    for o in objects:
        if o['type'] == 'polyline':
            if first:
                extra.x0 = o['points'][0][0]
                extra.y0 = o['points'][0][1]
                extra.x1 = extra.x0
                extra.y1 = extra.y0
                first = False

            for p in o['points']:
                x = p[0]
                y = p[1]

                if x < extra.x0: extra.x0 = x
                if y < extra.y0: extra.y0 = y
                if x > extra.x1: extra.x1 = x
                if y > extra.y1: extra.y1 = y

    # apply translation to all points, fontsizes, radiuses, etc...
    for o in objects:
        if o['type'] == 'polyline':
            for p in o['points']:
                p[0] -= extra.x0
                p[1] -= extra.y0
        elif o['type'] == 'text':
            o['position'][0] -= extra.x0
            o['position'][1] -= extra.y0

    doc = {}
    doc['info'] = {}
    doc['info']['width']  = extra.x1 - extra.x0
    doc['info']['height'] = extra.y1 - extra.y0
    doc['objects'] = objects

    json_dump(doc, stdout, indent=2)
