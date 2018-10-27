// TODO:
// 1. Handle depths
// 2. Handle areafill
// 3. Handle all types and subtypes
// 4. Handle penstyle
// 5. Handle style
// 6. Handle diffs
// 7. Build display list sorted by depth

var scale = 0.2;
var x1 = 0;
var y1 = 0;
var display_list = [];

function trans_x(p) {
    return (p - x1) * scale;
}

function trans_y(p) {
    return (p - y1) * scale;
}

function render_compound(ctx, obj) {
    render_element_list(ctx, obj.objects);
}

function render_polyline(ctx, obj) {
    ctx.lineWidth = obj.thickness;
    ctx.lineJoin = obj.joinstyle;
    ctx.lineCap = obj.capstyle;

    if (obj.subtype === "box") {
	obj_x = trans_x(obj.points[0][0]);
	obj_y = trans_y(obj.points[0][1]);
	obj_w = trans_x(obj.points[2][0]) - obj_x;
	obj_h = trans_y(obj.points[2][1]) - obj_y;

	if (obj.areafill == 20) {
	    ctx.fillStyle = obj.fillcolor;
	    ctx.fillRect(obj_x, obj_y, obj_w, obj_h);
	}

	if (obj.penstyle === -1) {
	    ctx.strokeStyle = obj.pencolor;
	    ctx.strokeRect(obj_x, obj_y, obj_w, obj_h);
	}
    }
    else if (obj.subtype === "polyline") {
	for (i in obj.points) {
	    if (i == 0) {
		ctx.moveTo(trans_x(obj.points[i][0]),
			   trans_y(obj.points[i][1]));
	    } else {
		ctx.lineTo(trans_x(obj.points[i][0]),
			   trans_y(obj.points[i][1]));
	    }
	}

	if (obj.areafill == 20) {
	    ctx.fillStyle = obj.fillcolor;
	    ctx.fill();
	}

	if (obj.penstyle === -1) {
	    ctx.strokeStyle = obj.pencolor;
	    ctx.stroke();
	}
    }
    else if (obj.subtype === "arc-box") {
	obj_x = trans_x(obj.points[0][0]);
	obj_y = trans_y(obj.points[0][1]);
	obj_w = trans_x(obj.points[2][0]) - obj_x;
	obj_h = trans_y(obj.points[2][1]) - obj_y;

	ctx.beginPath();
	ctx.moveTo(obj_x - obj.radius, obj_y);
	ctx.lineTo(obj_x + obj.radius + obj_w + obj.radius, obj_y);
	// ctx.arcTo(obj_x + obj.radius + obj_w
	ctx.closePath();

	if (obj.areafill == 20) {
	    ctx.fillStyle = obj.fillcolor;
	    ctx.fill();
	}

	if (obj.penstyle === -1) {
	    ctx.strokeStyle = obj.pencolor;
	    ctx.stroke();
	}
    }
    else {
    }
}

function render_element_list(ctx, l) {
    l1 = create_display_list(l);

    for (i in l1) {
	obj = l1[i];

	if (obj.type === "compound") {
	    render_compound(ctx, obj);
	} else if (obj.type === "polyline") {
	    render_polyline(ctx, obj);
	} else {
	}
    }
}

function flatten_element_list(l) {
    if (l.length == 0) {
	return [];
    } else if (l.length == 1) {
	if (l[0].type === "compound") {
	    return flatten_element_list(l[0].objects);
	} else {
	    return [l[0]];
	}
    } else {
	if (l[0].type === "compound") {
	    return flatten_element_list(l[0].objects).concat(
		flatten_element_list(l.slice(1)));
	} else {
	    return [l[0]].concat(flatten_element_list(l.slice(1)));
	}
    }
}

function create_display_list(l) {
    l1 = flatten_element_list(l);
    l1.sort(function(a, b) {
	return b.depth - a.depth;
    });
    return l1;
}

window.onload = function() {
    var canvas = document.getElementById("machine_canvas");
    var ctx = canvas.getContext("2d");

    x1 = machine.x1;
    y1 = machine.y1;

    canvas.width = (machine.x2 - x1) * scale;
    canvas.height = (machine.y2 - y1) * scale;

    render_element_list(ctx, machine.objects);
}
