#!/usr/bin/env python3
# Follows the FIG 3.2 format specification.
import extra
from json import dump as json_dump
from extra import log, die
from polyline import parse_polyline
from text import parse_text
from sys import stdout, stdin

def parse_object(line):
    tmpl = line.split(' ')

    if tmpl[0] == '#':
        res = parse_object(stdin.readline().strip())
        res['meta'] = tmpl[1]
        return res

    t = int(tmpl[0])

    if t == 2: return parse_polyline(tmpl) # polyline, polygon, box
    elif t == 4: return parse_text(tmpl) # text
    elif t == 6: # compound
        res = {}
        res['type'] = 'compound'
        res['children'] = []
        for l in stdin:
            ls = l.strip()
            if ls == '-6': break # end of compound
            res['children'] += [parse_object(ls)]
        return res
    else:
        die('type %s not supported' % typ)

def get_bounds(objs, first=False):
    # get width and height of document
    for o in objs:
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
        elif o['type'] == 'compound':
            get_bounds(o['children'])

def translate_objs(objs):
    # apply translation to all points, fontsizes, radiuses, etc...
    for o in objs:
        if o['type'] == 'polyline':
            for p in o['points']:
                p[0] -= extra.x0
                p[1] -= extra.y0
        elif o['type'] == 'text':
            o['pos'][0] -= extra.x0
            o['pos'][1] -= extra.y0
        elif o['type'] == 'compound':
            translate_objs(o['children'])

if __name__ == '__main__':
    objects = []

    # skip header
    for i in range(9): stdin.readline()

    # get objects
    for line in stdin:
        l = line.strip()
        if l.startswith('0'): pass # skip colors
        else: objects += [parse_object(l)]

    get_bounds(objects, first=True)
    translate_objs(objects)

    doc = dict(
        size=(extra.x1 - extra.x0, extra.y1 - extra.y0),
        thickness=max(int(extra.avg_thickness / extra.num_thickness), 1),
        fontsize=max(int(extra.avg_fontsize / extra.num_fontsize), 8),
        radius=max(int(extra.avg_radius / extra.num_radius), 5),
        objects=objects)
    json_dump(doc, stdout, indent=2)
