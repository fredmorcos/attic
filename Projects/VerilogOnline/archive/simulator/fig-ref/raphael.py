#!/usr/bin/python

# This is a FIG JSON -> Raphael format parser
# Part of the Computer Architecture Simulator

# Frederic-Gerald Morcos
# Institute for Computer Architecture
# Johannes Kepler University, Linz, Austria
# December 2011

# Flattens the list of objects from compounds into a sorted list by
# depth. Adds to each object an "attr" field with attributes that can
# directly be loaded into a Raphael paper (graphics context). Also,
# generates path strings for polylines and others.

import sys
import json

def sort_key(o):
    if "depth" in o.keys():
        return o["depth"]
    return 0

class RaphaelTranslator:
    def __init__(self):
        self.machine     = json.load(sys.stdin)
        self.objects     = None
        self.new_objects = None
        self.new_machine = None
        self.scale       = None

        self.properties = {
            "line_style": {"default"           : "",
                           "solid"             : "",
                           "dashed"            : "-",
                           "dotted"            : ".",
                           "dash-dotted"       : "-.",
                           "dash-double-dotted": "-..",
                           "dash-triple-dotted": "--.."}

                           ,

            "cap_style": {"butt"      : "butt",
                          "round"     : "round",
                          "projecting": "square"}

                          ,

            "text_just": {"left"  : "start",
                          "center": "middle",
                          "right" : "end"}

                          ,

            "arrow_type": {"stick"               : "open",
                           "closed-triangle"     : "block",
                           "closed-indented-butt": "classic",
                           "closed-pointed-butt" : "diamond"}}

    def translate(self):
        self.objects = sorted(
            self.flat_objs(self.machine["objects"]),
            # key=lambda o: o["depth"], reverse=True)
            key=sort_key, reverse=True)
        self.scale = self.machine["resolution"] / self.machine["magnification"]
        self.translate_attrs()
        self.new_machine = {"objects": self.new_objects,
                            "width"  : self.machine["x2"] - self.machine["x1"] + 600,
                            "height" : self.machine["y2"] - self.machine["y1"] + 600}
        print(json.dumps(self.new_machine, sort_keys=False, indent=4))

    def flat_objs(self, l):
        if len(l) == 0:
            return []

        obj = l[0]
        obj_type = obj["type"]
        tail = l[1:]

        if len(l) == 1:
            if obj_type == "compound":
                return self.flat_objs(obj["objects"]);
            else:
                return [obj]
        else:
            if obj_type == "compound":
                return self.flat_objs(obj["objects"]) + self.flat_objs(tail)
            else:
                return [obj] + self.flat_objs(tail)

    def prop(self, prop_dict, value):
        return self.properties[prop_dict][value]

    def get_arrow_width(self, value):
        # if value < 75:
        #     return "narrow"
        # elif value >= 75 and value < 150:
        #     return "midium"
        # else:
        #     return "wide"
        return "wide"

    def get_arrow_length(self, value):
        # if value < 100:
        #     return "short"
        # elif value >= 100 and value < 400:
        #     return "midium"
        # else:
        #     return "long"
        return "long"

    def translate_attrs_ellipse(self, o):
        res = {"type"            : "ellipse",
               "stroke-dasharray": self.prop("line_style", o["line_style"]),
               "stroke-width"    : o["thickness"] * self.scale,
               "stroke"          : o["pen_color"],
               "cx"              : o["center_x"],
               "cy"              : o["center_y"],
               "rx"              : o["radius_x"],
               "ry"              : o["radius_y"]}

        if o["area_fill"]:
            res.update({"fill": o["fill_color"]})

        return res

    def translate_attrs_polyline_box(self, o):
        res = {"type"            : "rect",
               "stroke-dasharray": self.prop("line_style", o["line_style"]),
               "stroke-width"    : o["thickness"] * self.scale,
               "stroke"          : o["pen_color"],
               "stroke-linejoin" : o["join_style"],
               "stroke-linecap"  : self.prop("cap_style", o["cap_style"]),
               "x"               : o["points"][0][0],
               "y"               : o["points"][0][1],
               "width"           : o["points"][2][0] - o["points"][0][0],
               "height"          : o["points"][2][1] - o["points"][0][1]}

        if o["area_fill"]:
            res.update({"fill": o["fill_color"]})

        return res

    def translate_attrs_polyline_arcbox(self, o):
        res = {"type"            : "rect",
               "stroke-dasharray": self.prop("line_style", o["line_style"]),
               "stroke-width"    : o["thickness"] * self.scale,
               "stroke"          : o["pen_color"],
               "stroke-linejoin" : o["join_style"],
               "stroke-linecap"  : self.prop("cap_style", o["cap_style"]),
               "x"               : o["points"][2][0],
               "y"               : o["points"][2][1],
               "width"           : o["points"][0][0] - o["points"][2][0],
               "height"          : o["points"][0][1] - o["points"][2][1],
               "r"               : o["radius"] * self.scale}

        if o["area_fill"]:
            res.update({"fill": o["fill_color"]})

        return res

    def translate_attrs_polyline_polygon(self, o):
        res = {"type"            : "rect",
               "stroke-dasharray": self.prop("line_style", o["line_style"]),
               "stroke-width"    : o["thickness"] * self.scale,
               "stroke"          : o["pen_color"],
               "stroke-linejoin" : o["join_style"],
               "stroke-linecap"  : self.prop("cap_style", o["cap_style"])}

        if o["area_fill"]:
            res.update({"fill": o["fill_color"]})

        for i, p in enumerate(o["points"]):
            if i == 0:
                path_str = "M{},{}".format(str(p[0]), str(p[1]))
            else:
                path_str += "L{},{}".format(str(p[0]), str(p[1]))
        path_str += "Z"

        res.update({"path": path_str})
        return res

    def translate_attrs_polyline_polyline(self, o):
        res = {"type"            : "path",
               "stroke-dasharray": self.prop("line_style", o["line_style"]),
               "stroke-width"    : o["thickness"] * self.scale,
               "stroke"          : o["pen_color"],
               "stroke-linejoin" : o["join_style"],
               "stroke-linecap"  : self.prop("cap_style", o["cap_style"])}

        if o["f_arrow"]:
            info = o["f_arrow_info"]
            res.update({"arrow-start": "{}-{}-{}".format(
                self.prop("arrow_type", info["type"]),
                self.get_arrow_width(info["width"]),
                self.get_arrow_length(info["height"]))})

        if o["b_arrow"]:
            info = o["b_arrow_info"]
            res.update({"arrow-end": "{}-{}-{}".format(
                self.prop("arrow_type", info["type"]),
                self.get_arrow_width(info["width"]),
                self.get_arrow_length(info["height"]))})

        for i, p in enumerate(reversed(o["points"])):
            if i == 0:
                path_str = "M{},{}".format(str(p[0]), str(p[1]))
            else:
                path_str += "L{},{}".format(str(p[0]), str(p[1]))

        res.update({"path": path_str})
        return res

    def translate_attrs_polyline(self, o):
        obj_trans_funcs = {"polyline": self.translate_attrs_polyline_polyline,
                           "box"     : self.translate_attrs_polyline_box,
                           "arc-box" : self.translate_attrs_polyline_arcbox,
                           "polygon" : self.translate_attrs_polyline_polygon}

        return obj_trans_funcs[o["subtype"]](o)

    def translate_attrs_spline(self, o):
        res = {"type"            : "path",
               "stroke-dasharray": self.prop("line_style", o["line_style"]),
               "stroke-width"    : o["thickness"] * self.scale,
               "stroke-linecap"  : self.prop("cap_style", o["cap_style"]),
               "stroke"          : o["pen_color"]}

        if o["f_arrow"]:
            info = o["f_arrow_info"]
            res.update({"arrow-start": "{}-{}-{}".format(
                self.prop("arrow_type", info["type"]),
                self.get_arrow_width(info["width"]),
                self.get_arrow_length(info["height"]))})

        if o["b_arrow"]:
            info = o["b_arrow_info"]
            res.update({"arrow-end": "{}-{}-{}".format(
                self.prop("arrow_type", info["type"]),
                self.get_arrow_width(info["width"]),
                self.get_arrow_length(info["height"]))})

        if o["area_fill"]:
            res.update({"fill": o["fill_color"]})

        for i, p in enumerate(o["points"]):
            if i == 0:
                path_str = "M{},{}".format(str(p[0]), str(p[1]))
            else:
                path_str += "T{},{}".format(str(p[0]), str(p[1]))

        res.update({"path": path_str})
        return res

    def translate_attrs_text(self, o):
        return {"type"       : "text",
                "font-size"  : o["font_size"] * self.scale,
                "stroke"     : o["color"],
                "text"       : o["text"],
                "text-anchor": self.prop("text_just", o["subtype"]),
                "x"          : o["coord_x"],
                "y"          : o["coord_y"]}

    def translate_attrs_arc(self, o):
        res = {"type"            : "path",
               "stroke-dasharray": self.prop("line_style", o["line_style"]),
               "stroke-width"    : o["thickness"] * self.scale,
               "stroke-linecap"  : self.prop("cap_style", o["cap_style"]),
               "stroke"          : o["pen_color"]}

        if o["f_arrow"]:
            info = o["f_arrow_info"]
            res.update({"arrow-start": "{}-{}-{}".format(
                self.prop("arrow_type", info["type"]),
                self.get_arrow_width(info["width"]),
                self.get_arrow_length(info["height"]))})

        if o["b_arrow"]:
            info = o["b_arrow_info"]
            res.update({"arrow-end": "{}-{}-{}".format(
                self.prop("arrow_type", info["type"]),
                self.get_arrow_width(info["width"]),
                self.get_arrow_length(info["height"]))})

        if o["area_fill"]:
            res.update({"fill": o["fill_color"]})

        path_str = "M{},{}L{},{}L{},{}".format(
            o["point_x1"], o["point_y1"],
            o["point_x2"], o["point_y2"],
            o["point_x3"], o["point_y3"])

        res.update({"path": path_str})
        return res

    def translate_attrs(self):
        obj_trans_funcs = {"ellipse" : self.translate_attrs_ellipse,
                           "polyline": self.translate_attrs_polyline,
                           "spline"  : self.translate_attrs_spline,
                           "text"    : self.translate_attrs_text,
                           "arc"     : self.translate_attrs_arc}

        self.new_objects = []

        for o in self.objects:
            if o["type"] != "color":
                self.new_objects += [obj_trans_funcs[o["type"]](o)]

if __name__ == "__main__":
    rt = RaphaelTranslator()
    rt.translate()
