#!/usr/bin/env python3
# converts from fig-based json -> semantic json
import json
import sys

from math import pi
from uuid import uuid4 as uid

RADIUS = 0
FONTSIZE = 0
THICKNESS = 0

def _die(msg): _log(msg); exit(1);
def _log(msg): sys.stderr.write(msg + '\n');
def _escapeStr(s): return s.replace('%', '_').replace('-', '_')

_getValueLabels = lambda children, metaname:
    [c for c in children if c['type'] == 'text' and c['string'] == metaname]
_getTitleLabels = lambda children, metaname:
    [c for c in children if c['type'] == 'text' and c['string'] != metaname]
_getOtherElems = lambda children: [c for c in children if c['type'] != 'text']

def _getBoxCoords(box):
    xPoints = [i[0] for i in box['points']]
    yPoints = [i[1] for i in box['points']]
    minX, maxX = min(xPoints), max(xPoints)
    minY, maxY = min(yPoints), max(yPoints)
    w, h = abs(maxX - minX), abs(maxY - minY)
    return minX, minY, w, h

def _factorElems(elems):
    for e in elems:
        t = e['type']

        if t == 'text':
            e['x'] = e['pos'][0]
            e['y'] = e['pos'][1]
            del e['pos']

        if t == 'box' or t == 'arcbox':
            coords = _getBoxCoords(e)
            e['x'], e['y'] = coords[0], coords[1]
            e['width'], e['height'] = coords[2], coords[3]
            del e['points']

        if t == 'arcbox':
            e['type'] = 'box'
            e['rounded'] = True
            e['radius'] = RADIUS

        if t == 'polyline':
            arrows = {}
            if 'farrow' in e and e['farrow'] == True:
                arrows['start'] = 1
                del e['farrow']

            if 'barrow' in e and e['barrow'] == True:
                arrows['end'] = 1
                del e['barrow']

            e['arrows'] = arrows

if __name__ == '__main__':
    global RADIUS
    global FONTSIZE
    global THICKNESS

    try: img = json.load(sys.stdin)
    except KeyboardInterrupt: _die('\nabort due to user interrupt')

    if 'objects' not in img: _die('image has no objects section')

    _log('creating new image...')

    RADIUS = img['radius']
    FONTSIZE = img['fontsize']
    THICKNESS = img['thickness']

    nodes = []
    canvas = dict(id=uid(), type='canvas',
                  width=img['size'][0], height=img['size'][1])
    newImg = dict(objects=[canvas], states=[])

    for o in img['objects']:
        t = o['type']

        if t == 'compound':
            c = o['children']

            if 'meta' in o:
                m = o['meta']

                valueLabels = _delType(_getValueLabels(c, m))
                titleLabels = _delType(_getTitleLabels(c, m))
                otherElems  = _factorElems(_getOtherElems(c))

    _log('done')
    json.dump(newImg, sys.stdout, indent=2)
