import extra as e
from sys import stdin
from math import pi

def parseText(tmp_line):
    obj_type = 'text'

    # SubType: 0=left justified, 1=center justified, 2=right justified
    subtype = int(tmp_line[1])
    if    subtype == 0: subtype = 'left'
    elif  subtype == 1: subtype = 'center'
    elif  subtype == 2: subtype = 'right'
    else: e.die('subtype %s not supported' % subtype)

    color      = int(tmp_line[2])          # NOT USED
    depth      = int(tmp_line[3]) - 50     # default depth value
    penstyle   = int(tmp_line[4])          # NOT USED
    font       = int(tmp_line[5])          # NOT USED
    font_size  = float(tmp_line[6]) * 12.0 # a magic number
    angle      = float(tmp_line[7])
    font_flags = int(tmp_line[8])          # NOT USED, bit vector

    # text height and length
    height = float(tmp_line[9])
    length = float(tmp_line[10])

    # Position (of text anchor), depends on SubType
    # if SubType = 0 -> position is bottom left corner
    # if SubType = 1 -> position is bottom center
    # if SubType = 2 -> position is bottom right corner
    x, y = int(tmp_line[11]), int(tmp_line[12])

    string = ''
    for i in range(13, len(tmp_line)): string += tmp_line[i] + ' '
    string = string.strip()[:-4]

    if font_size > 1:
        e.avg_fontsize += font_size
        e.num_fontsize += 1

    res = {'type': obj_type,
           # 'depth': depth,
           # 'subtype': subtype,
           # 'align': subtype,
           # 'fontsize': font_size,
           # 'angle': angle,
           # 'height': height,
           # 'length': length,
           'pos': [x, y],
           'string': string}

    if depth   != 0:        res['depth'] = depth
    if angle   != 0.0:      res['angle'] = int(angle * 180 / pi)
    if subtype != 'center': res['align'] = subtype
    return res
