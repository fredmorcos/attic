#!/usr/bin/env python
# -*- coding: utf-8 -*-

# gtkcairoplot.py
#
# Copyright (c) 2008 Magnun Leno da Silva
#
# Author: Magnun Leno da Silva <magnun.leno@gmail.com>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA

import pygtk
pygtk.require('2.0')
import gtk, gobject, cairo, cairoplot


class gtk_plot(gtk.DrawingArea):
    # Draw in response to an expose-event
    __gsignals__ = { "expose-event": "override" }
    def __init__(self):
        gtk.DrawingArea.__init__(self)
        self.plot = None
        self.args = {}
        self.resizable = True
    
    def set_args(self, args):
        # Get only valid keys and alter the defaul values
        intersection = set(self.args.keys()) & set(args.keys())
        for occurence in intersection:
            self.args[occurence] = args[occurence]
        
        # Find not matching arg keys and raise an exception
        not_matching = set(args.keys()) - set(self.args.keys())
        if len(not_matching) > 0:
            raise NameError, "Unknown arg keys: "+str(list(not_matching))
        
        width = self.args['width']
        height = self.args['height']
        
        # Set the minimum size for the image
        self.set_size_request(width, height)
    
    def do_expose_event(self, event):
        # Create the cairo context
        cr = self.window.cairo_create()
        
        if self.resizable:
            width, height = self.window.get_size()
            self.args['width'] = width
            self.args['height'] = height
        self.function(cr, **self.args)    
    
    
#### Start of the widgets for each Plot ####
# Class for the CairoPlot horizontal_bar_plot
class gtk_horizontal_bar_plot(gtk_plot):
    def __init__(self):
        gtk_plot.__init__(self)
        self.function = cairoplot.horizontal_bar_plot
        self.args = {'data'               : None,
                     'width'              : 640,
                     'height'             : 480,
                     'background'         : "white light_gray",
                     'border'             : 0,
                     'display_values'     : False,
                     'grid'               : False,
                     'rounded_corners'    : False,
                     'stack'              : False,
                     'three_dimension'    : False,
                     'series_labels'      : None,
                     'x_labels'           : None,
                     'y_labels'           : None,
                     'x_bounds'           : None,
                     'y_bounds'           : None,
                     'colors'             : None}

# Class for the CairoPlot function_plot
class gtk_function_plot(gtk_plot):
    def __init__(self):
        gtk_plot.__init__(self)
        self.function = cairoplot.function_plot
        self.args = {'data'               : None,
                     'width'              : 640,
                     'height'             : 480,
                     'background'         : "white light_gray",
                     'border'             : 0,
                     'axis'               : True,
                     'dots'               : False,
                     'discrete'           : False,
                     'grid'               : False,
                     'series_legend'      : False,
                     'x_labels'           : None,
                     'y_labels'           : None,
                     'x_bounds'           : None,
                     'y_bounds'           : None,
                     'x_title'            : None,
                     'y_title'            : None,
                     'series_colors'      : None,
                     'step'               : 1}

# Class for the CairoPlot gantt_chart
class gtk_gantt_chart(gtk_plot):
    def __init__(self):
        gtk_plot.__init__(self)
        self.function = cairoplot.gantt_chart
        self.args = {'pieces'             : None,
                     'width'              : 640,
                     'height'             : 480,
                     'x_labels'           : None,
                     'y_labels'           : None,
                     'colors'             : None}

# Class for the CairoPlot donut_plot
class gtk_donut_plot(gtk_plot):
    def __init__(self):
        gtk_plot.__init__(self)
        self.function = cairoplot.donut_plot
        self.args = {'data'               : None,
                     'width'              : 640,
                     'height'             : 480,
                     'background'         : "white light_gray",
                     'gradient'           : False,
                     'shadow'             : False,
                     'colors'             : None,
                     'inner_radius'       : -1}

# Class for the CairoPlot pie_plot
class gtk_pie_plot(gtk_plot):
    def __init__(self):
        gtk_plot.__init__(self)
        self.function = cairoplot.pie_plot
        self.args = {'data'               : None,
                     'width'              : 640,
                     'height'             : 480,
                     'background'         : "white light_gray",
                     'gradient'           : False,
                     'shadow'             : False,
                     'colors'             : None}

# Class for the CairoPlot dot_line_plot
class gtk_dot_line_plot(gtk_plot):
    def __init__(self):
        gtk_plot.__init__(self)
        self.function = cairoplot.dot_line_plot
        self.args = {'data'               : None,
                     'width'              : 640,
                     'height'             : 480,
                     'background'         : "white light_gray",
                     'border'             : 0,
                     'axis'               : False,
                     'dash'               : False,
                     'dots'               : False,
                     'grid'               : False,
                     'series_legend'      : False,
                     'x_labels'           : None,
                     'y_labels'           : None,
                     'x_bounds'           : None,
                     'y_bounds'           : None,
                     'x_title'            : None,
                     'y_title'            : None,
                     'series_colors'      : None}

# Class for the CairoPlot scatter_plot
class gtk_scatter_plot(gtk_plot):
    def __init__(self):
        gtk_plot.__init__(self)
        self.function = cairoplot.scatter_plot
        self.args = {'data'               : None,
                     'errorx'             : None,
                     'errory'             : None,
                     'width'              : 640,
                     'height'             : 480,
                     'background'         : "white light_gray",
                     'border'             : 0,
                     'axis'               : False,
                     'dash'               : False,
                     'discrete'           : False,
                     'dots'               : False,
                     'grid'               : False,
                     'series_legend'      : False,
                     'x_labels'           : None,
                     'y_labels'           : None,
                     'x_bounds'           : None,
                     'y_bounds'           : None,
                     'z_bounds'           : None,
                     'x_title'            : None,
                     'y_title'            : None,
                     'series_colors'      : None,
                     'circle_colors'      : None}

# Class for the CairoPlot stream_chart
class gtk_stream_chart(gtk_plot):
    def __init__(self):
        gtk_plot.__init__(self)
        self.function = cairoplot.stream_chart
        self.args = {'data'               : None,
                     'width'              : 640,
                     'height'             : 480,
                     'background'         : "white light_gray",
                     'border'             : 0,
                     'grid'               : False,
                     'series_legend'      : None,
                     'x_labels'           : None,
                     'x_bounds'           : None,
                     'y_bounds'           : None,
                     'colors'             : None}

# Class for the CairoPlot vertical_bar_plot
class gtk_vertical_bar_plot(gtk_plot):
    def __init__(self):
        gtk_plot.__init__(self)
        self.function = cairoplot.vertical_bar_plot
        self.args = {'data'               : None,
                     'width'              : 640,
                     'height'             : 480,
                     'background'         : "white light_gray",
                     'border'             : 0,
                     'display_values'     : False,
                     'grid'               : False,
                     'rounded_corners'    : False,
                     'stack'              : False,
                     'three_dimension'    : False,
                     'series_labels'      : None,
                     'x_labels'           : None,
                     'y_labels'           : None,
                     'x_bounds'           : None,
                     'y_bounds'           : None,
                     'colors'             : None}

if __name__ == '__main__':
    import tests
    tests.run()
