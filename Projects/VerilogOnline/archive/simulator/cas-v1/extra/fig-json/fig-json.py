#!/usr/bin/env python3
# Follows the FIG 3.2 format specification.
import extra as e

from extra import log, die

from polyline import parsePolyline
from text import parseText

from json import dump as json_dump

from sys import stdout
from sys import stdin

def _parseObject(line):
    tmpl = line.split(' ')

    if tmpl[0] == '#':
        res = _parseObject(stdin.readline().strip())
        res['meta'] = tmpl[1]
        return res

    t = int(tmpl[0])

    if t == 2:   return parsePolyline(tmpl) # polyline, polygon, box
    elif t == 4: return parseText(tmpl)     # text
    elif t == 6:                            # compound
        res = {}
        res['type'] = 'compound'
        res['children'] = []

        for l in stdin:                     # keep reading compound
            ls = l.strip()
            if ls == '-6': break            # end of compound
            res['children'] += [_parseObject(ls)]
        return res
    else: die('type %s not supported' % typ)

def _getBounds(objs, first=False):
    # get width and height of document
    for o in objs:
        if o['type'] == 'polyline':
            if first:
                e.x0, e.y0 = o['points'][0][0], o['points'][0][1]
                e.x1, e.y1 = e.x0, e.y0
                first = False

            for p in o['points']:
                x, y = p[0], p[1]

                if x < e.x0: e.x0 = x
                if y < e.y0: e.y0 = y
                if x > e.x1: e.x1 = x
                if y > e.y1: e.y1 = y
        elif o['type'] == 'compound': _getBounds(o['children'])

def _translateObjects(objs):
    # apply translation to all points, fontsizes, radiuses, etc...
    for o in objs:
        if o['type'] == 'polyline':
            for p in o['points']:
                p[0] -= e.x0
                p[1] -= e.y0
        elif o['type'] == 'text':
            o['pos'][0] -= e.x0
            o['pos'][1] -= e.y0
        elif o['type'] == 'compound': _translateObjects(o['children'])

if __name__ == '__main__':
    objects = []
    for i in range(9): stdin.readline() # skip header

    for line in stdin:                  # parse objects
        l = line.strip()
        if l.startswith('0'): pass      # skip colors
        else: objects += [_parseObject(l)]

    _getBounds(objects, first=True)
    _translateObjects(objects)

    doc = dict(size      = [e.x1 - e.x0, e.y1 - e.y0],
               thickness = max(e.avg_thickness // e.num_thickness, 1),
               fontsize  = max(e.avg_fontsize  // e.num_fontsize,  8),
               radius    = max(e.avg_radius    // e.num_radius,    5),
               objects   = objects)
    json_dump(doc, stdout, indent=2, sort_keys=True)
