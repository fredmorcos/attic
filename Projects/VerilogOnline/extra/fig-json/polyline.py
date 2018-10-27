import extra
from extra import die
from sys import stdin

def parse_polyline(tmp_line): # polyline, polygon, box
    obj_type = 'polyline'

    # SubType: 1=polyline, 2=box, 3=polygon, 4=arc-box, 5=pic
    subtype = int(tmp_line[1])
    if    subtype == 1: subtype = 'polyline'
    elif  subtype == 2: subtype = 'box'
    elif  subtype == 3: subtype = 'polygon'
    elif  subtype == 4: subtype = 'arcbox'
    else: die('subtype %s not supported' % subtype)

    # LineStyle: -1=Default, 0=Solid, 1=Dashed, 2=Dotted, 3=Dash-dotted,
    #            4=Dash-double-dotted, 5=Dash-triple-dotted
    linestyle = int(tmp_line[2])
    if    linestyle == -1: linestyle = 'default'
    elif  linestyle == 0:  linestyle = 'solid'
    else: die('linestyle %s not supported' % linestyle)

    thickness = int(tmp_line[3]) * 12
    if thickness == 0:
        visible = False
    else:
        visible = True

    pencolor = int(tmp_line[4])   # NOT USED
    fillcolor = int(tmp_line[5])  # NOT USED
    depth = int(tmp_line[6])      # USED: 0...999
    penstyle = int(tmp_line[7])   # NOT USED
    areafill = int(tmp_line[8])   # NOT USED, -1=not filled
    styleval = float(tmp_line[9]) # NOT USED

    # JoinStyle: 0=Miter, 1=Round, 2=Bevel
    joinstyle = int(tmp_line[10]) # NOT USED

    # ONLY FOR POLYLINE
    # CapStyle: 0=Butt, 1=Round, 2=Projecting
    capstyle = int(tmp_line[11]) # NOT USED

    # ONLY FOR ARCBOX
    # radius of arc-box
    radius = int(tmp_line[12]) * 12

    # ONLY FOR POLYLINE
    # ForwardArrow, BackwardArrow: 0=off, 1=on
    forwardarrow = bool(int(tmp_line[13]))
    backwardarrow = bool(int(tmp_line[14]))

    # NPoints: number of points in line
    npoints = int(tmp_line[15]) # USED ONLY HERE

    if subtype == 'polyline':
        # ignore arrow details, we'll draw them all the same
        if forwardarrow:
            for l in stdin: # skip forwardarrow line
                break
        if backwardarrow:
            for l in stdin: # skip backwardarrow line
                break

    total = 0
    points = []
    for tmp_points_line in stdin:
        points_line = tmp_points_line.strip().split(' ')

        i = 0
        while i < len(points_line):
            x = int(points_line[i])
            y = int(points_line[i + 1])

            points += [[x, y]]

            i += 2
            total += 2

        if (total / 2) == npoints:
            break

    if radius > 0:
        extra.avg_radius += radius
        extra.num_radius += 1

    if thickness > 0:
        extra.avg_thickness += thickness
        extra.num_thickness += 1

    res = {'type': subtype,
           # 'type': obj_type,
           # 'subtype': subtype,
           # 'linestyle': linestyle,
           # 'thickness': thickness,
           # 'visible': visible,
           # 'depth': depth,
           # 'radius': radius,
           # 'farrow': forwardarrow,
           # 'barrow': backwardarrow,
           'points': points}

    if forwardarrow:  res['farrow'] = True
    if backwardarrow: res['barrow'] = True

    return res
