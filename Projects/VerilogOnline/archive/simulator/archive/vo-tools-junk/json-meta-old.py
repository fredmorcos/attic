#!/usr/bin/env python3

import json, sys

def die(msg):
    log(msg)
    exit(1)

def log(msg):
    sys.stderr.write(msg + '\n')

def pack_common_values(machine):
    log('averaging out values for fontsize, thickness, radius')
    avg_fontsize = 0
    num_fontsize = 0
    avg_thickness = 0
    num_thickness = 0
    avg_radius = 0
    num_radius = 0

    for k,v in machine['metaobjects'].items():
        for o in v:
            if 'fontsize' in o:
                avg_fontsize += o['fontsize']
                num_fontsize += 1
                del o['fontsize']
            if 'thickness' in o:
                avg_thickness += o['thickness']
                num_thickness += 1
                del o['thickness']
            if 'radius' in o:
                avg_radius += o['radius']
                num_radius += 1
                del o['radius']

    for o in machine['objects']:
        if 'fontsize' in o:
            avg_fontsize += o['fontsize']
            num_fontsize += 1
            del o['fontsize']
        if 'thickness' in o:
            avg_thickness += o['thickness']
            num_thickness += 1
            del o['thickness']
        if 'radius' in o:
            avg_radius += o['radius']
            num_radius += 1
            del o['radius']

    avg_fontsize /= num_fontsize
    avg_thickness /= num_thickness
    avg_radius /= num_radius

    machine['info']['fontsize'] = avg_fontsize
    machine['info']['thickness'] = avg_thickness
    machine['info']['radius'] = avg_radius

def compress_single_metaobjects(machine):
    log('trying to compress single metaobjects')

    to_del = []

    for k,v in machine['metaobjects'].items():
        if len(v) == 1:
            log('single metaobject found')
            machine['objects'] += [v[0]]
            to_del += [k]

    for i in to_del:
        del machine['metaobjects'][i]

def remove_useless_properties(o):
    if 'visible'   in o: del o['visible']
    if 'visited'   in o: del o['visited']
    if 'depth'     in o: del o['depth']
    if 'linestyle' in o: del o['linestyle']
    if 'metaname'  in o: del o['metaname']

    if o['type'] == 'polyline':
        if o['subtype'] != 'arcbox':
            del o['radius']

        if o['subtype'] == 'arcbox':
            o['subtype'] = 'box'

        if o['subtype'] == 'arcbox' or o['subtype'] == 'box':
            newpoints = [o['points'][0], o['points'][-3]]
            o['points'] = newpoints
            del o['forwardarrow']
            del o['backwardarrow']
    elif o['type'] == 'text':
        del o['length']
        del o['height']
    else:
        die('unknown type %s' % o['type'])

if __name__ == '__main__':
    try:
        obj = json.load(sys.stdin)
    except KeyboardInterrupt:
        die('\nabort due to user interrupt')

    newobj = {}

    if 'info'    not in obj: die('image has no info section')
    if 'objects' not in obj: die('image has no objects section')

    newobj['info'] = obj['info']
    newobj['metaobjects'] = {}
    newobj['objects'] = []

    log('packing metaobjects')
    for o in obj['objects']:
        if o['type'] == 'polyline' and o['visible'] == False:
            log('skipping invisible object (outer)')
            continue

        if 'metaname' in o and 'visited' not in o:
            name = o['metaname']
            newobj['metaobjects'][name] = []

            for o2 in obj['objects']:
                if o['type'] == 'polyline' and o['visible'] == False:
                    log('skipping invisible object (inner)')
                    continue

                if 'metaname' in o2 and o2['metaname'] == name and \
                       'visited' not in o2:
                    newobj['metaobjects'][name] += [o2]
                    o2['visited'] = True

    log('packing remaining normal objects')
    for o in obj['objects']:
        if o['type'] == 'polyline' and o['visible'] == False:
            log('skipping invisible object')
            continue
        if 'visited' not in o:
            newobj['objects'] += [o]

    log('removing useless properties from metaobjects')
    for k,v in newobj['metaobjects'].items():
        for o in v:
            remove_useless_properties(o)

    log('removing useless properties from normal objects')
    for o in newobj['objects']:
        remove_useless_properties(o)

    compress_single_metaobjects(newobj)
    pack_common_values(newobj);

    log('done')
    json.dump(newobj, sys.stdout, indent=2)
