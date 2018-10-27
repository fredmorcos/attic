#!/usr/bin/env python3

from pprint import PrettyPrinter
from argparse import ArgumentParser

def parse_image_header(input_file):
    comment = None
    
    for num, line in enumerate(input_file):
        line = line.strip()
        if num == 0:
            # First line, has to be a comment with the version, but we don't care
            continue
        elif num == 1:
            # Orientation = Landscape | Portrait
            orientation = line
        elif num == 2:
            # Justification = Center | Flush Left
            justification = line
        elif num == 3:
            # Units = Metric | Inches
            units = line
        elif num == 4:
            # PaperSize = Letter | Legal | Ledger | Tabloid |
            #             A | B | C | D | E | A4 | A3 | A2 | A1 | A0 | B5
            papersize = line
        elif num == 5:
            # Magnification = FLOAT
            magnification = float(line)
        elif num == 6:
            # MultiPage = Single | Multiple
            multipage = (line == 'Multiple')
        elif num == 7:
            # TransparentColor = -3=bg | -2=none | -1=default |
            #                    0-31=stdcolors | 32-=usercolors
            transpcolor = int(line)
        elif line.startswith('#'):
            # optional comment, if not, then resolution
            comment += line
            comment += '\n'
        else:
            # Resolution = units/inch & CoordSystem = 1=LowerLeft | 2=UpperLeft
            tmp_line = line.split(' ')
            resolution = int(tmp_line[0])
            coordsystem = int(tmp_line[1])
            break # VERY IMPORTANT

    return {'orientation':   orientation,
            'justification': justification,
            'units':         units,
            'papersize':     papersize,
            'magnification': magnification,
            'multipage':     multipage,
            'transpcolor':   transpcolor,
            'comment':       comment,
            'resolution':    resolution,
            'coordsystem':   coordsystem}

def parse_objects(line, input_file):
    objects = []
    tmp_line = line.split(' ')
    if int(tmp_line[0]) == 2: # polyline, polygon, box
        # SubType: 1=polyline, 2=box, 3=polygon, 4=arc-box, 5=pic
        subtype = int(tmp_line[1])

        # LineStyle: -1=Default, 0=Solid, 1=Dashed, 2=Dotted, 3=Dash-dotted,
        #            4=Dash-double-dotted, 5=Dash-triple-dotted
        linestyle = int(tmp_line[2])

        thickness = int(tmp_line[3])
        pencolor = int(tmp_line[4])
        fillcolor = int(tmp_line[5])
        depth = int(tmp_line[6])      # 0...999
        penstyle = int(tmp_line[7])   # NOT USED
        areafill = int(tmp_line[8])   # -1=not filled
        styleval = float(tmp_line[9])

        # 0=Miter, 1=Round, 2=Bevel
        joinstyle = int(tmp_line[10])

        # ONLY FOR POLYLINE
        # 0=Butt, 1=Round, 2=Projecting
        capstyle = int(tmp_line[11])

        radius = int(tmp_line[12])

        # 0=off, 1=on
        forwardarrow = int(tmp_line[13])
        backwardarrow = int(tmp_line[14])

        npoints = int(tmp_line[15])
        
    for line in input_file:
        pass
    return objects

if __name__ == '__main__':
    argparser = ArgumentParser(description='Convert a FIG file')
    argparser.add_argument('input', help='Input FIG file to convert')
    argparser.add_argument('output', help='Output file')
    argparser.add_argument('format', help='Output format')
    args = argparser.parse_args()

    header = None
    pseudocolors = {}
    objects = []

    with open(args.input, 'r') as input_file:
        header = parse_image_header(input_file)
        for line in input_file:
            line = line.strip()
            if line.startswith('0'):
                tmp_line = line.split(' ')
                pseudocolors[int(tmp_line[1])] = tmp_line[2]
            else:
                objects = parse_objects(line, input_file)

    pp = PrettyPrinter(indent=2)
    pp.pprint(header)
    pp.pprint(pseudocolors)
    pp.pprint(objects)
