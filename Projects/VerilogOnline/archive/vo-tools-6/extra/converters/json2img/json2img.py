#!/usr/bin/python2

import cairo
from math import pi, atan2
from sys import stdin, stdout, stderr
from json import loads as json_loads
from argparse import ArgumentParser

def get_arrow_angle(p1, p2):
    x1 = p1[0]
    y1 = p1[1]
    x2 = p2[0]
    y2 = p2[1]
    dx = x2 - x1
    dy = y2 - y1

    rad = pi / 180.0

    if dx == 0:
        if y2 > y1: return 90.0 * rad
        else:       return 270.0  * rad
    else:
        if dy == 0:
            if x2 > x1: return 0.0   * rad
            else:       return 180.0 * rad
        else:
            a = atan2(dy, dx)

            if x2 > x1:
                if y2 > y1: return a + (360.0 * rad)
                else:       return a
            else:
                if y2 > y1: return a + (360.0 * rad)
                else:       return a
    return None

def draw_arrow(cr, pos, angle):
    arrow_size = 50
    x = pos[0]
    y = pos[1]

    cr.save()
    cr.translate(x, y)
    cr.rotate(angle)
    cr.move_to(-arrow_size * 2, -arrow_size)
    cr.line_to(-arrow_size * 2, arrow_size)
    cr.line_to(0, 0)
    cr.line_to(-arrow_size * 2, -arrow_size)
    cr.close_path()
    cr.stroke_preserve()
    cr.fill()
    cr.restore()

if __name__ == '__main__':
    argp = ArgumentParser(description='Convert a JSON image to SVG')
    argp.add_argument('output', help='Output filename')
    argp.add_argument('--type', type=str, metavar='T', default='png',
                      help='Select output type (png, svg)')
    argp.add_argument('--scaling', type=float, metavar='S', default=0.25,
                      help='Scaling factor (0.0 < S <= 1.0)')
    args = argp.parse_args()

    if args.scaling <= 0.0 or args.scaling > 1.0:
        stderr.write('bad scaling factor value')
        exit(1)

    if args.type == 'png' or args.type == 'svg':
        pass
    else:
        stderr.write('unknown output image type given')
        exit(1)

    ip  = stdin.read()
    doc = json_loads(ip)

    width  = doc['info']['width']
    height = doc['info']['height']

    if args.type == 'png':
        surface = cairo.ImageSurface(cairo.FORMAT_ARGB32,
                                     int(width * args.scaling),
                                     int(height * args.scaling))
    else:
        surface = cairo.SVGSurface(args.output,
                                   width * args.scaling,
                                   height * args.scaling)

    cr = cairo.Context(surface)
    cr.scale(args.scaling, args.scaling)
    cr.set_source_rgb(1.0, 1.0, 1.0)
    cr.rectangle(0, 0, width, height)
    cr.fill()
    cr.set_source_rgb(0.0, 0.0, 0.0)
    cr.select_font_face('monospace')

    for o in doc['objects']:
        if o.has_key('changed'):
            cr.set_source_rgb(1.0, 0.0, 0.0)
        else:
            cr.set_source_rgb(0.0, 0.0, 0.0)

        if o['type'] == 'text':
            cr.save()
            cr.set_font_size(o['fontsize'])

            x = o['position'][0]
            y = o['position'][1]

            cr.move_to(x, y)
            cr.rotate(-o['angle'])
            cr.show_text(o['string'])
            cr.restore()
        elif o['type'] == 'polyline':
            if o['visible'] == False:
                continue

            cr.set_line_width(o['thickness'])

            if o['subtype'] == 'arcbox':
                points = o['points']
                r = -(o['radius'])

                x0 = points[0][0]
                y0 = points[0][1]
                x1 = points[-3][0]
                y1 = points[-3][1]

                cr.curve_to(x0 + r, y0, x0, y0, x0, y0 + r)
                cr.line_to(x0, y1 - r)
                cr.curve_to(x0, y1 - r, x0, y1, x0 + r, y1)
                cr.line_to(x1 - r, y1)
                cr.curve_to(x1 - r, y1, x1, y1, x1, y1 - r)
                cr.line_to(x1, y0 + r)
                cr.curve_to(x1, y0 + r, x1, y0, x1 - r, y0)
                cr.line_to(x0 + r, y0)

                cr.close_path()
                cr.stroke()
            else:
                cr.move_to(o['points'][0][0], o['points'][0][1])
                for p in o['points'][1:]:
                    cr.line_to(p[0], p[1])
                if o['subtype'] == 'box':
                    cr.close_path()
                cr.stroke()

                # arrows
                if o['forwardarrow']:
                    p1 = o['points'][-2]
                    p2 = o['points'][-1]
                    draw_arrow(cr, p2, get_arrow_angle(p1, p2))

                if o['backwardarrow']:
                    p1 = o['points'][1]
                    p2 = o['points'][0]
                    draw_arrow(cr, p2, get_arrow_angle(p1, p2))
        else:
            sys.stderr.write('type (%s) not supported' % o['type'])
            exit(1)

    if args.type == 'png':
        surface.write_to_png(args.output)
