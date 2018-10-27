// TODO:
// 2. Handle areafill
// 3. Handle all types and subtypes
// 4. Handle penstyle
// 5. Handle style
// 6. Handle diffs

var scale = 0;

function render_machine(p, m) {
    p.clear();

    for (i in m.objects_flat) {
	obj = m.objects_flat[i];

	if (obj.type === "polyline") {
	    render_polyline(p, obj);
	} else if (obj.type === "text") {
	    render_text(p, obj);
	} else {
	}
    }
}

function render_text(p, obj) {
    p.text(obj.coord_x, obj.coord_y, obj.text).attr(obj.attrs).attr(
	{"font-size": obj.font_size * scale});
}

function render_polyline(p, obj) {
    if (obj.subtype === "box") {
	obj_x = obj.points[0][0];
	obj_y = obj.points[0][1];
	obj_w = obj.points[2][0] - obj_x;
	obj_h = obj.points[2][1] - obj_y;

	e = p.rect(obj_x, obj_y, obj_w, obj_h);
    } else if (obj.subtype === "polyline" || obj.subtype === "polygon") {
	e = p.path();
    } else if (obj.subtype === "arc-box") {
	obj_x = obj.points[2][0];
	obj_y = obj.points[2][1];
	obj_w = obj.points[0][0] - obj_x;
	obj_h = obj.points[0][1] - obj_y;

	e = p.rect(obj_x, obj_y, obj_w, obj_h, obj.radius * scale);
    } else {
	e = null;
    }

    e.attr(obj.attrs);
    e.attr({"stroke-width": obj.thickness * scale});
}

window.onload = function() {
    var w = machine.x2 - machine.x1 + 200;
    var h = machine.y2 - machine.y1 + 200;

    scale = machine.resolution / machine.magnification;

    var paper = Raphael(document.getElementById("machine_div"), w, h);

    render_machine(paper, machine);
}
