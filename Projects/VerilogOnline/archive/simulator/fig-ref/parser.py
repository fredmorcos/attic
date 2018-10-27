#!/usr/bin/python

# This is a FIG -> Human Readable JSON parser
# Part of the Computer Architecture Simulator

# Frederic-Gerald Morcos
# Institute for Computer Architecture
# Johannes Kepler University, Linz, Austria
# December 2011

# TODO:
# 2. Do not ignore Pen style.
# 3. Do not ignore Style.
# 4. Use color codes instead of names.

import sys
import json

class CannotFindTranslatorException(Exception):
    def __init__(self, translator):
        self.translator = translator

    def __str__(self):
        return "Cannot find translator '{}'".format(
            self.translator)

class NotATranslatorException(Exception):
    def __init__(self, translator):
        self.translator = translator

    def __str__(self):
        return "Plugin '{}' is not a translator".format(
            self.translator)

class BadFIGException(Exception):
    def __init__(self, line_number, message):
        self.message     = message
        self.line_number = line_number

    def __str__(self):
        return "Line {}: {}".format(
            repr(self.line_number),
            self.message)

class EOFIGException(Exception):
    def __init__(self):
        pass

class EOCompoundException(Exception):
    def __init__(self):
        pass

class NotYetSupportedException(Exception):
    def __init__(self, message):
        self.message = message

    def __str__(self):
        return message

def smaller(x, y):
    if x == None:
        return y
    elif y == None:
        return x
    elif x < y:
        return x
    else:
        return y

def larger(x, y):
    if x == None:
        return y
    elif y == None:
        return x
    elif x > y:
        return x
    else:
        return y

class FIGParser:
    def __init__(self, translator_name = "none"):
        self.translator  = None
        self.line_number = 0
        self.document    = {}

        self.x1 = None
        self.y1 = None
        self.x2 = None
        self.y2 = None

        self.load_translator(translator_name)

        self.properties = {
            "line_style": {-1: "default",
                            0:  "solid",
                            1:  "dashed",
                            2:  "dotted",
                            3:  "dash-dotted",
                            4:  "dash-double-dotted",
                            5:  "dash-triple-dotted"}

                            ,
                            
            "join_style": {0: "miter",
                           1: "round",
                           2: "bevel"}

                           ,

            "cap_style": {0: "butt",
                          1: "round",
                          2: "projecting"}

                          ,

            "arrow_type": {0: "stick",
                           1: "closed-triangle",
                           2: "closed-indented-butt",
                           3: "closed-pointed-butt"}

                           ,

            "arrow_style": {0: "hollow",
                            1: "filled"}

                            ,

            "direction": {0: "clockwise",
                          1: "counterclockwise"}

                          ,

            "area_fill": {-1: False,
                          20: True}

                          ,

            "color": {-1: "black",    # Default
                       0: "black",
                       1: "blue",
                       2: "green",
                       3: "cyan",
                       4: "red",
                       5: "magenta",
                       6: "yellow",
                       7: "white",
                       8: "blue",     # Shades of Blue (Dark -> Light)
                       9: "blue",
                      10: "blue",
                      11: "blue",
                      12: "green",    # Shades of Green (Dark -> Light)
                      13: "green",
                      14: "green",
                      15: "cyan",     # Shades of Cyan (Dark -> Light)
                      16: "cyan",
                      17: "cyan",
                      18: "red",      # Shades of Red (Dark -> Light)
                      19: "red",
                      20: "red",
                      21: "magenta",  # Shades of Magenta (Dark -> Light)
                      22: "magenta",
                      23: "magenta",
                      24: "brown",    # Shades of Brown (Dark -> Light)
                      25: "brown",
                      26: "brown",
                      27: "pink",     # Shades of Pink (Dark -> Light)
                      28: "pink",
                      29: "pink",
                      30: "pink",
                      31: "gold"}

                      ,

            # For font_flags bit 2 = 1 (PostScript fonts):
            "ps_font": {-1: "Default font",
                         0: "Times Roman",
                         1: "Times Italic",
                         2: "Times Bold",
                         3: "Times Bold Italic",
                         4: "AvantGarde Book",
                         5: "AvantGarde Book Oblique",
                         6: "AvantGarde Demi",
                         7: "AvantGarde Demi Oblique",
                         8: "Bookman Light",
                         9: "Bookman Light Italic",
                        10: "Bookman Demi",
                        11: "Bookman Demi Italic",
                        12: "Courier",
                        13: "Courier Oblique",
                        14: "Courier Bold",
                        15: "Courier Bold Oblique",
                        16: "Helvetica",
                        17: "Helvetica Oblique",
                        18: "Helvetica Bold",
                        19: "Helvetica Bold Oblique",
                        20: "Helvetica Narrow",
                        21: "Helvetica Narrow Oblique",
                        22: "Helvetica Narrow Bold",
                        23: "Helvetica Narrow Bold Oblique",
                        24: "New Century Schoolbook Roman",
                        25: "New Century Schoolbook Italic",
                        26: "New Century Schoolbook Bold",
                        27: "New Century Schoolbook Bold Italic",
                        28: "Palatino Roman",
                        29: "Palatino Italic",
                        30: "Palatino Bold",
                        31: "Palatino Bold Italic",
                        32: "Symbol",
                        33: "Zapf Chancery Medium Italic",
                        34: "Zapf Dingbats"}

                        ,

            # For font_flags bit 2 = 0 (LaTeX fonts):
            "latex_font": {0: "Default font",
                           1: "Roman",
                           2: "Bold",
                           3: "Italic",
                           4: "Sans Serif",
                           5: "Typewriter"}}

    def prop(self, value, prop_dict):
        assert(type(value) == int)
        assert(type(prop_dict) == str)

        if self.translator != None:
            return self.translator.prop(
                value, prop_dict, self.properties[prop_dict][value])
        else:
            return self.properties[prop_dict][value]

    def load_translator(self, name):
        assert(type(name) == str)

        if name == "none":
            return
        else:
            try:
                mod = __import__(name)
                self.translate = mod.Translator()
            except ImportError:
                raise CannotFindTranslatorException(name)
            except AttributeError:
                raise NotATranslatorException(name)

    def get_area_fill(self, value):
        assert(type(value) == int)

        if value not in [-1, 20]:
            raise NotYetSupportedException("Area fill value not supported")
        else:
            return self.area_fill[value]

    def get_font_info(self, value, flags):
        assert(type(value) == int)
        assert(type(flags) == int)

        font_info = {
            "rigid"     : bool((flags & 0x1) >> 0),
            "special"   : bool((flags & 0x2) >> 1),
            "postscript": bool((flags & 0x4) >> 2),
            "hidden"    : bool((flags & 0x8) >> 3)}

        if font_info["postscript"]:
            font_info.update({"font": self.properties["ps_font"][value]})
        else:
            font_info.update({"font": self.properties["latex_font"][value]})

        return font_info

    def parse_input(self):
        objects_list = []

        self.document.update({
            "version"      : self.next_token(self.read_version),
            "orientation"  : self.next_token(self.read_orientation),
            "justification": self.next_token(self.read_justification),
            "units"        : self.next_token(self.read_units),
            "papersize"    : self.next_token(self.read_papersize),
            "magnification": self.next_token(self.read_magnification),
            "multipage"    : self.next_token(self.read_multipage),
            "transparent"  : self.next_token(self.read_transparent)})

        comment, next_line = self.read_comments()
        resolution, coord  = self.read_resolution_coord(next_line)

        self.document.update({
            "comment"   : comment,
            "resolution": resolution,
            "coord"     : coord})

        while True:
            try:
                objects_list += [self.read_object()]
            except EOFIGException:
                break

        self.document.update({"objects": objects_list,
                              "x1": self.x1,
                              "y1": self.y1,
                              "x2": self.x2,
                              "y2": self.y2})

        print(json.dumps(self.document, sort_keys=False, indent=4))

    def read_props_color(self, properties):
        assert(type(properties) == list)

        return {"number": int(properties[1]),
                "color" : properties[2]}

    def read_props_compound(self, properties):
        assert(type(properties) == list)

        objects_list = []

        obj = {"upperleft_x" : int(properties[1]),
               "upperleft_y" : int(properties[2]),
               "lowerright_x": int(properties[3]),
               "lowerright_y": int(properties[4])}

        while True:
            try:
                objects_list += [self.read_object()]
            except EOFIGException:
                raise BadFIGException(
                    self.line_number,
                    "File ended while in Compound element")
            except EOCompoundException:
                break

        obj.update({"objects": objects_list})

        return obj

    def read_props_spline(self, properties):
        assert(type(properties) == list)

        subtypes = {0: "open-approximated",
                    1: "closed-approximated",
                    2: "open-interpolated",
                    3: "closed-interpolated",
                    4: "open-x",
                    5: "closed-x"}

        obj = {"subtype"   : subtypes[int(properties[1])],
               "line_style": self.prop(int(properties[2]), "line_style"),
               "thickness" : int(properties[3]),
               "pen_color" : self.prop(int(properties[4]), "color"),
               "fill_color": self.prop(int(properties[5]), "color"),
               "depth"     : int(properties[6]),
               "pen_style" : int(properties[7]),
               "area_fill" : self.prop(int(properties[8]), "area_fill"),
               "style"     : float(properties[9]),
               "cap_style" : self.prop(int(properties[10]), "cap_style"),
               "f_arrow"   : bool(int(properties[11])),
               "b_arrow"   : bool(int(properties[12])),
               "n_points"  : int(properties[13])}

        self.read_arrow_lines(obj)

        obj.update(self.read_points_line(obj["n_points"]))
        obj.update(self.read_spline_points_line(obj["n_points"]))

        self.got_new_points(obj["points"])

        return obj

    def read_props_ellipse(self, properties):
        assert(type(properties) == list)

        subtypes = {1: "ellipse-radii",
                    2: "ellipse-diameters",
                    3: "circle-radius",
                    4: "circle-diameter"}

        obj = {"subtype"   : subtypes[int(properties[1])],
               "line_style": self.prop(int(properties[2]), "line_style"),
               "thickness" : int(properties[3]),
               "pen_color" : self.prop(int(properties[4]), "color"),
               "fill_color": self.prop(int(properties[5]), "color"),
               "depth"     : int(properties[6]),
               "pen_style" : int(properties[7]),
               "area_fill" : self.prop(int(properties[8]), "area_fill"),
               "style"     : float(properties[9]),
               "direction" : self.prop(int(properties[10]), "direction"),
               "angle"     : float(properties[11]),
               "center_x"  : int(properties[12]),
               "center_y"  : int(properties[13]),
               "radius_x"  : int(properties[14]),
               "radius_y"  : int(properties[15]),
               "start_x"   : int(properties[16]),
               "start_y"   : int(properties[17]),
               "end_x"     : int(properties[18]),
               "end_y"     : int(properties[19])}

        self.got_new_points([(obj["center_x"], obj["center_y"]),
                             (obj["center_x"] + obj["radius_x"],
                              obj["center_y"] + obj["radius_y"]),
                             (obj["center_x"] - obj["radius_x"],
                              obj["center_y"] - obj["radius_y"]),
                             (obj["start_x"], obj["start_y"]),
                             (obj["end_x"], obj["end_y"])])

        return obj

    def read_props_text(self, properties):
        assert(type(properties) == list)

        subtypes = {0: "left",
                    1: "center",
                    2: "right"}

        obj = {"subtype"    : subtypes[int(properties[1])],
               "color"      : self.prop(int(properties[2]), "color"),
               "depth"      : int(properties[3]),
               "pen_style"  : int(properties[4]),
               "_font"      : int(properties[5]),
               "font_size"  : float(properties[6]),
               "angle"      : float(properties[7]),
               "_font_flags": int(properties[8]),
               "height"     : float(properties[9]),
               "length"     : float(properties[10]),
               "coord_x"    : int(properties[11]),
               "coord_y"    : int(properties[12])}

        obj.update(self.get_font_info(obj["_font"], obj["_font_flags"]))

        obj_string = ""
        obj_string_list = properties[13:]

        for element in obj_string_list:
            element = element.replace("\\\\n", "\n")
            if element.endswith("\\001"):
                obj_string += element[:-4]
            else:
                obj_string += element + " "

        obj.update({"text": obj_string})

        self.got_new_points([(obj["coord_x"], obj["coord_y"]),
                             (obj["coord_x"] + int(obj["length"]),
                              obj["coord_y"] + int(obj["height"]))])

        return obj

    def read_props_polyline(self, properties):
        assert(type(properties) == list)

        subtypes = {1: "polyline",
                    2: "box",
                    3: "polygon",
                    4: "arc-box",
                    5: "picture"}

        obj = {"subtype"   : subtypes[int(properties[1])],
               "line_style": self.prop(int(properties[2]), "line_style"),
               "thickness" : int(properties[3]),
               "pen_color" : self.prop(int(properties[4]), "color"),
               "fill_color": self.prop(int(properties[5]), "color"),
               "depth"     : int(properties[6]),
               "pen_style" : int(properties[7]),
               "area_fill" : self.prop(int(properties[8]), "area_fill"),
               "style"     : float(properties[9]),
               "join_style": self.prop(int(properties[10]), "join_style"),
               "cap_style" : self.prop(int(properties[11]), "cap_style"),
               "radius"    : int(properties[12]),
               "f_arrow"   : bool(int(properties[13])),
               "b_arrow"   : bool(int(properties[14])),
               "n_points"  : int(properties[15])}

        self.read_arrow_lines(obj)

        if obj["subtype"] == "picture":
            obj.update(self.read_picture_line(self.next_line().split(" ")))

        obj.update(self.read_points_line(obj["n_points"]))

        self.got_new_points(obj["points"])

        return obj

    def read_props_arc(self, properties):
        assert(type(properties) == list)

        subtypes = {1: "open-ended",
                    2: "pie-wedge"}

        obj = {"subtype"   : subtypes[int(properties[1])],
               "line_style": self.prop(int(properties[2]), "line_style"),
               "thickness" : int(properties[3]),
               "pen_color" : self.prop(int(properties[4]), "color"),
               "fill_color": self.prop(int(properties[5]), "color"),
               "depth"     : int(properties[6]),
               "pen_style" : int(properties[7]),
               "area_fill" : self.prop(int(properties[8]), "area_fill"),
               "style"     : float(properties[9]),
               "cap_style" : self.prop(int(properties[10]), "cap_style"),
               "direction" : self.prop(int(properties[11]), "direction"),
               "f_arrow"   : bool(int(properties[12])),
               "b_arrow"   : bool(int(properties[13])),
               "center_x"  : float(properties[14]),
               "center_y"  : float(properties[15]),
               "point_x1"  : int(properties[16]),
               "point_y1"  : int(properties[17]),
               "point_x2"  : int(properties[18]),
               "point_y2"  : int(properties[19]),
               "point_x3"  : int(properties[20]),
               "point_y3"  : int(properties[21])}

        self.read_arrow_lines(obj)

        self.got_new_points([(int(obj["center_x"]), int(obj["center_y"])),
                             (obj["point_x1"], obj["point_y1"]),
                             (obj["point_x2"], obj["point_y2"]),
                             (obj["point_x3"], obj["point_y3"])])

        return obj

    def got_new_point(self, x, y):
        assert(type(x) == int)
        assert(type(y) == int)

        self.x1 = smaller(self.x1, x)
        self.y1 = smaller(self.y1, y)
        self.x2 = larger(self.x2, x)
        self.y2 = larger(self.y2, y)

    def got_new_points(self, points_list):
        assert(type(points_list) == list)

        for point in points_list:
            self.got_new_point(point[0], point[1])

    def got_new_points_flat(self, points_list):
        assert(type(points_list) == list)
        assert(len(points_list) % 2 == 0)

        self.got_new_points(zip([e for e in points_list if e % 2 == 1],
                                [e for e in points_list if e % 2 == 0]))

    def read_arrow_lines(self, obj):
        assert(type(obj) == dict)

        if obj["f_arrow"]:
            obj.update({"f_arrow_info": self.read_arrow_line()})

        if obj["b_arrow"]:
            obj.update({"b_arrow_info": self.read_arrow_line()})

    def read_arrow_line(self):
        properties = self.next_line().split(" ")
        return {"type"     : self.prop(int(properties[0]), "arrow_type"),
                "style"    : self.prop(int(properties[1]), "arrow_style"),
                "thickness": float(properties[2]),
                "width"    : float(properties[3]),
                "height"   : float(properties[4])}

    def read_spline_points_line(self, points_count):
        assert(type(points_count) == int)

        spoints = []

        i = 0
        while i < points_count:
            split_line = self.next_line().split(" ")

            j = 0
            while j < len(split_line):
                spoints += [float(split_line[j])]
                j += 1

            i += j

        return {"s_points": spoints}

    def read_points_line(self, points_count):
        assert(type(points_count) == int)

        points = []

        i = 0
        while i < points_count:
            split_line = self.next_line().split(" ")

            j = 0
            while j < len(split_line):
                points += [(int(split_line[j]),
                            int(split_line[j+1]))]
                j += 2

            i += (j // 2)

        return {"points": points}

    def read_picture_line(self, picture_line):
        assert(type(picture_line) == list)

        return {"picture_props":
                {"flipped": bool(int(picture_line[0])),
                 "file"   : picture_line[1]}}

    def read_object(self):
        obj_props = {0: ("color",    self.read_props_color),
                     1: ("ellipse",  self.read_props_ellipse),
                     2: ("polyline", self.read_props_polyline),
                     3: ("spline",   self.read_props_spline),
                     4: ("text",     self.read_props_text),
                     5: ("arc",      self.read_props_arc),
                     6: ("compound", self.read_props_compound)}

        fig_object = {}

        comment, next_line = self.read_comments()
        fig_object.update({"comment": comment})

        split_next_line = next_line.split(" ")

        obj_code = int(split_next_line[0])

        if obj_code == -6: # End of Compound
            raise EOCompoundException()
        
        fig_object.update({"type": obj_props[obj_code][0]})
        fig_object.update(obj_props[obj_code][1](split_next_line))

        return fig_object

    def read_comments(self):
        comment = ""
        line = self.next_line()

        while (line[0] == "#"):
            if comment != "":
                comment += "\n"
            comment += line

            line = self.next_line()

        return (comment, line)

    def next_token(self, func):
        assert(type(func) == type(self.next_token) or
               type(func) == type(print))

        line = self.next_line()
        return func(line)

    def next_line(self):
        line = sys.stdin.readline()

        if not line:
            raise EOFIGException()

        stripped_line = line.strip()

        self.line_number += 1

        if len(stripped_line) == 0:
            return self.next_line()
        else:
            return line.strip()

    def read_justification(self, line):
        assert(type(line) == str)

        if line in ["Center", "Flush Left"]:
            return line
        else:
            raise BadFIGException(
                self.line_number, "Bad justification value")

    def read_units(self, line):
        assert(type(line) == str)

        if line in ["Metric", "Inches"]:
            return line
        else:
            raise BadFIGException(
                self.line_number, "Bad units value")

    def read_orientation(self, line):
        assert(type(line) == str)

        if line in ["Landscape", "Portrait"]:
            return line
        else:
            raise BadFIGException(
                self.line_number, "Bad orientation value")

    def read_version(self, line):
        assert(type(line) == str)

        split_line = line.split(" ")

        if split_line[0] == "#FIG":
            try:
                value = float(split_line[1])
                return value
            except ValueError:
                raise BadFIGException(
                    self.line_number, "Cannot parse FIG version")
        else:
            raise BadFIGException(
                self.line_number, "First line not a '#FIG <version>'")

    def read_papersize(self, line):
        assert(type(line) == str)

        if line in ["Letter", "Legal", "Ledger", "Tabloid",
                    "A"     , "B"    , "C"     , "D"      , "E" ,
                    "A4"    , "A3"   , "A2"    , "A1"     , "A0", "B5"]:
            return line
        else:
            raise BadFIGException(
                self.line_number, "Bad papersize value")

    def read_magnification(self, line):
        assert(type(line) == str)

        try:
            value = float(line)
            return value
        except ValueError:
            raise BadFIGException(
                self.line_number, "Cannot parse magnification value")

    def read_multipage(self, line):
        assert(type(line) == str)

        if line in ["Single", "Multiple"]:
            return line
        else:
            raise BadFIGException(
                self.line_number, "Bad multipage value")

    def read_transparent(self, line):
        assert(type(line) == str)

        try:
            value = int(line)
            return line
        except ValueError:
            raise BadFIGException(
                self.line_number, "Cannot parse transparent color value")

    def read_resolution_coord(self, line):
        assert(type(line) == str)

        split_line = line.split(" ")

        try:
            resolution_value = int(split_line[0])

            try:
                coord_value = int(split_line[1])
                return (resolution_value, coord_value)
            except ValueError:
                raise BadFIGException(
                    self.line_number, "Cannot parse resolution value")
        except ValueError:
            raise BadFIGException(
                self.line_number, "Cannot parse coordinate value")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        fig_parser = FIGParser(sys.argv[1])
    else:
        fig_parser = FIGParser()
    fig_parser.parse_input()
