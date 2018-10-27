import extra as e
from sys import stdin

def parsePolyline(tmp_line): # polyline, polygon, box
    obj_type = 'polyline'

    # SubType: 1=polyline, 2=box, 3=polygon, 4=arc-box, 5=pic
    subtype = int(tmp_line[1])
    if    subtype == 1: subtype = 'polyline'
    elif  subtype == 2: subtype = 'box'
    elif  subtype == 3: subtype = 'polygon'
    elif  subtype == 4: subtype = 'arcbox'
    else: e.die('subtype %s not supported' % subtype)

    # LineStyle: -1=Default, 0=Solid, 1=Dashed, 2=Dotted, 3=Dash-dotted,
    #            4=Dash-double-dotted, 5=Dash-triple-dotted
    linestyle = int(tmp_line[2])
    if    linestyle == -1: linestyle = 'default'
    elif  linestyle == 0:  linestyle = 'solid'
    else: e.die('linestyle %s not supported' % linestyle)

    thickness = int(tmp_line[3]) * 12
    visible   = (thickness > 0)

    pencolor  = int(tmp_line[4])       # NOT USED
    fillcolor = int(tmp_line[5])       # NOT USED
    depth     = int(tmp_line[6]) - 50  # USED: 0...999
    penstyle  = int(tmp_line[7])       # NOT USED
    areafill  = int(tmp_line[8])       # NOT USED, -1=not filled
    styleval  = float(tmp_line[9])     # NOT USED

    # JoinStyle: 0=Miter, 1=Round, 2=Bevel
    joinstyle = int(tmp_line[10]) # NOT USED

    # ONLY FOR POLYLINE
    # CapStyle: 0=Butt, 1=Round, 2=Projecting
    capstyle = int(tmp_line[11]) # NOT USED

    # ONLY FOR ARCBOX
    radius = int(tmp_line[12]) * 12 # radius of arc-box

    # ONLY FOR POLYLINE
    # ForwardArrow, BackwardArrow: 0=off, 1=on
    farrow = bool(int(tmp_line[13]))
    barrow = bool(int(tmp_line[14]))

    # NPoints: number of points in line
    npoints = int(tmp_line[15]) # USED ONLY HERE

    # ignore arrow details, we draw them all the same
    if subtype == 'polyline':
        if farrow: stdin.readline() # skip farrow line
        if barrow: stdin.readline() # skip farrow line

    total = 0
    points = []
    for tmp_points_line in stdin:
        points_line = tmp_points_line.strip().split(' ')

        for i in range(0, len(points_line), 2):
            points += [[int(points_line[i]), int(points_line[i + 1])]]
            total  += 1

        if total == npoints: break

    if radius > 0:
        e.avg_radius += radius
        e.num_radius += 1

    if thickness > 0:
        e.avg_thickness += thickness
        e.num_thickness += 1

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

    if depth != 0: res['depth']  = depth
    if farrow:     res['farrow'] = True
    if barrow:     res['barrow'] = True
    return res
