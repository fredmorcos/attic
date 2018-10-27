import logging as log

from modules.extra import die

def _jsonSetElementState(element, change, canToggle):
    # Sets state information for a single elements, just a helper function
    if change:    element['highlight']  = True
    if canToggle: element['can_toggle'] = canToggle

def _jsonReplacePattern(img, pat, txt, change=False, canToggle=False):
    # Finds the item with string matching pattern and replaces it with text
    if pat in img['objects']:
        for o in img['metaobjects'][pat]:
            _jsonSetElementState(o, change, canToggle)
            if o['type'] == 'text' and o['string'] == pat:
                o['string']  = txt
                o['isvalue'] = True
    else:
        for o in img['objects']:
            if o['type'] == 'text' and o['string'] == pat:
                _jsonSetElementState(o, change, False)
                o['string'] = txt

def _jsonInsertExternal(img, pat, data):
    # Looks for the corresponding metaobject and sets the submachine data to it
    if pat in img['objects']:
        img['extobjects'][pat]            = {}
        img['extobjects'][pat]['objects'] = img['metaobjects'][pat]
        img['extobjects'][pat]['data']    = data
        del img['metaobjects'][pat]
    else:
        log.error('pattern = %s' % pat)
        die('trying to set sub-machine data to non-metaobject')

def jsonExport(machine):
    # Outputs to JSON files for rendering in a web browser (client-side).
    img = machine['image']
    log.info('updating objects in machine: %s' % machine['title'])
    _jsonReplacePattern(img, '%TITLE%', machine['title'])

    for k,v in machine.get('elements', {}).items():
        _jsonReplacePattern(img, k, v['value'], v['changed'], v['can_toggle'])

    for k,v in machine.get('mem_elements', {}).items():
        mem_data = ''
        for line in v['value']:
            mem_data += line + '\n'
        mem_data.strip()

        _jsonReplacePattern(img, k, mem_data)

    img['extobjects'] = {}

    for k,v in machine.get('sub_elements', {}).items():
        _jsonReplacePattern(img, k, v['title'])
        _jsonInsertExternal(img, k, jsonExport(v))

    return img
