from extra import log_and_die

def parse_text(tmp_line, input_file): # text
    obj_type = 'text'

    # SubType: 0=left justified, 1=center justified, 2=right justified
    subtype = int(tmp_line[1])
    if    subtype == 0: subtype = 'left'
    elif  subtype == 1: subtype = 'center'
    elif  subtype == 2: subtype = 'right'
    else: log_and_die('subtype %s not supported' % subtype)

    color = int(tmp_line[2])

    depth = int(tmp_line[3])      # USED: 0...999
    penstyle = int(tmp_line[4])   # NOT USED
    font = int(tmp_line[5])       # NOT USED BUT HAS TO BE MONOSPACE
    font_size = float(tmp_line[6]) * 12.0
    angle = float(tmp_line[7])
    font_flags = int(tmp_line[8]) # NOT USED, bit vector

    # text height and length
    height = float(tmp_line[9])
    length = float(tmp_line[10])

    # Position (of text anchor), depends on SubType
    # if SubType = 0 -> position is bottom left corner
    # if SubType = 1 -> position is bottom center
    # if SubType = 2 -> position is bottom right corner
    x = int(tmp_line[11])
    y = int(tmp_line[12])

    string = ''
    i = 13
    while i < len(tmp_line):
        string += tmp_line[i] + ' '
        i += 1
    string = string.strip()[:-4]

    return {'type': obj_type,
            'subtype': subtype,
            'depth': depth,
            'fontsize': font_size,
            'angle': angle,
            'height': height,
            'length': length,
            'position': [x, y],
            'string': string}
