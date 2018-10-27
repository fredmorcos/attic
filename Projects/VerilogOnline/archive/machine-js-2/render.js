function Machine(thecanvas) {

	var objects = {};
	var windows = [];

	var canvasList = [];	// TODO
	var windowList = [];

	var scenes = [];
	var step = 0;

	var polyline_constructor = function(settings, core) {

		return oCanvas.extend({
			core: core,

			shapeType: "rectangular",

            //_: oCanvas.extend({}, core.displayObject._, {}),

			init: function() {
				//this._.initialized = true;
				this.setGravity();
				log(this);
			},

			setGravity: function() {
				var sx = sy = s = dsx = dsy = ds = 0;
				var t = b = this.points[0][1], r = l = this.points[0][0];
				for (var i = 1; i < this.points.length; i++) {
					// Part length
					dsx = this.points[i][0] - this.points[i-1][0];
					dsy = this.points[i][1] - this.points[i-1][1];
					ds = Math.sqrt(dsx*dsx + dsy*dsy);
					// center of mass
					dsx = (this.points[i][0] + this.points[i-1][0])/2;
					dsy = (this.points[i][1] + this.points[i-1][1])/2;
					sx += dsx*ds;
					sy += dsy*ds;
					s += ds;

					// Frame
					if (this.points[i][0] > r) r = this.points[i][0];
					if (this.points[i][0] < l) l = this.points[i][0];
					if (this.points[i][1] < t) t = this.points[i][1];
					if (this.points[i][1] > b) b = this.points[i][1];

				}
				sx = 1/s*sx;
				sy = 1/s*sy;
				this.width = r - l;
				this.height = b - t;
				this.bounding = { tl: { x: l, y: t }, br: { x: r, y: b } };
				this.x = this.bounding.tl.x;
				this.y = this.bounding.tl.y;
				this.sx = sx;
				this.sy = sy;
			},

			isPointerInside: function() {
				var inside = this.core.tools.isPointerInside(this, this.core.pointer);
				if (!inside) return false;

				// Check point in polygon
				//     http://alienryderflex.com/polygon/
				var px = this.core.pointer.x, py = this.core.pointer.y;
				var i, sides = this.points.length, j = sides - 1;
				var odd = false;
				for (i = 0; i < sides; i++) {
					if (this.points[i][1] < py && this.points[j][1] >= py
							|| this.points[j][1] < py && this.points[i][1] >= py) {
						if (this.points[i][0] + (py - this.points[i][1]) / (this.points[j][1] - this.points[i][1]) * this.points[j][0] - this.points[i][0] < px) {
							odd = !odd;
						}
					}
					j = i;
				}
				return !odd;
			},

			draw: function() { 
				var canvas = this.core.canvas;
				var x = this.x,
					y = this.y;

				canvas.beginPath();
				canvas.strokeStyle = this.strokeColor;
				canvas.lineWidth = this.strokeWidth;

				canvas.moveTo(this.points[0][0] - x, this.points[0][1] - y);
				for (var i = 1; i < this.points.length; i++) {
					canvas.lineTo(this.points[i][0] - x, this.points[i][1] - y);
				}
				canvas.stroke();
				canvas.closePath();

				if (this.fill !== "") {
					canvas.fillStyle = this.fill;
					canvas.fill();
				}
			}
		}, settings);
	};

	oCanvas.registerDisplayObject("polyline", polyline_constructor, "init");

	// Create an html node
	function appendNode(data, parent_node) {
		switch (data.type) {
		case "window":
			var id = data.id;
			if (!id) 
				id = 'dialog';	// FIXME: create something unique
			var obj = $('<div />').attr('id', id).addClass('machine-subwindow');
			obj.type = "window";
			objects[id] = obj;
			if (data.title) 
				obj.attr('title', data.title);
			$(parent_node).append(obj);
			obj.dialog(oCanvas.extend({ autoOpen: false }, data));
			break;

		case "html":
			var obj = $(data.content);
			$(parent_node).append(obj);
			break;

		case "canvas":
			var id = data.id;
			if (!id) id = 'canvas'; // FIXME: create something unique
			var obj = $('<canvas />').attr('id', id).appendTo(parent_node);
			var c = oCanvas.create({ canvas: '#'+id });
			c.width = data.width;
			c.height = data.height;
			obj.type = 'canvas';
			if (data.nodes) {
				var nodes = data.nodes;
				for (var i = 0; i < nodes.length; i++) {
					var node = createCanvasNode(nodes[i], c);
				}
			}
			break;

		default:
			console.log("Unknown data");
			console.log(data);
			return false;
		}
		
		if (obj && obj.type !== 'canvas') {
			// Add children
			if (data.nodes) {
				var nodes = data.nodes;
				for (var i = 0; i < nodes.length; i++) {
					appendNode(nodes[i], obj);
				}
			}
		}
		return obj;
	}

	// Add a node to canvas
	function createCanvasNode(data, parent_node) {
		var obj = null;
		var defaults = {
			strokeWidth: 1,
			strokeColor: "black"
		};
		
		switch (data.type) {
		case "box":
		case "rectangle":
			defaults = oCanvas.extend(defaults, { width: 100, height: 100 });
			var obj = parent_node.display.rectangle(oCanvas.extend(defaults, data));
			break;
		case "circle":
			var obj = parent_node.display.ellipse(oCanvas.extend(defaults, data));
			break;
		case "polyline":
			var obj = parent_node.display.polyline(oCanvas.extend(defaults, data));
			break;
		case "line":
			var obj = parent_node.display.line(oCanvas.extend(defaults, data));
			break;
		default:
			console.log("Unknown canvas data");
			console.log(data);
			return false;
		}

		if (!obj) return null;
		parent_node.addChild(obj);

		// Add ids
		if (data.id) {
			objects[data.id] = obj;
		}
		
		// check for labels
		var label = null;
		if (data.label) {
			var defaults = {
				align: "center",
				baseline: "middle",
				fill: obj.strokeColor,
				x: obj.width/2,
				y: obj.height/2,
				opacity: obj.opacity
			};
			if (typeof data.label == 'string') {
				var text_data = defaults;
				if (data.type == "line") {
					text_data.align = "left";
					text_data.x = 5;
				}
				text_data.text = data.label;
				var text = parent_node.display.text(text_data);
				obj.addChild(text);
				if (data.id) {
					obj.label = text;
				}
				label = text;
			} else if (typeof data.label == 'object') {
				var count = data.label.length;
				for (var i in data.label) {
					var text_data = defaults;
					text_data.text = data.label[i];
					var text = parent_node.display.text(text_data);
					text.y = text.y - (count-1)*text.size/2 + i*text.size;
					obj.addChild(text);
				}
			}

		}
		
		// Add events
		var hlevent = false;
		if (data.click) {
			hlevent = true;
			obj.bind("click", function() {
				eval(data.click);
				this.redraw();
			});
			if (label) {
				label.bind("click", function() {
					eval(data.click);
					this.redraw();
				});
			}
		}
		if (hlevent) {
			obj.bind("mouseenter", function() {
				if (data.enter) 
					eval(data.enter);
				this.shadow = "0 0 5px #aaa";
				parent_node.mouse.cursor("pointer");
				parent_node.redraw();
			});
			obj.bind("mouseleave", function() {
				if (data.leave) 
					eval(data.leave);
				this.shadowBlur = 0;
				parent_node.mouse.cursor("default");
				parent_node.redraw();
			});
			if (label) {
				label.bind("mouseenter", function() {
					if (obj.isPointerInside()) {
						return;
					}
					if (data.enter) 
						eval(data.enter);
					obj.shadow = "0 0 5px #aaa";
					parent_node.mouse.cursor("pointer");
					parent_node.redraw();
				});
				label.bind("mouseleave", function() {
					if (obj.isPointerInside()) {
						return;
					}
					if (data.leave) 
						eval(data.leave);
					obj.shadowBlur = 0;
					parent_node.mouse.cursor("default");
					parent_node.redraw();
				});
			}
		}


		return obj;
	};

	// get element 
	function getObject(obj) {
		if (typeof obj == "string") {
			if (objects[obj]) {
				return objects[obj];
			} else {
				console.log("Unknown object " + obj);
			}
		} else if (typeof obj == "object") {
			return obj;
		}
		return null;
	}

	// Element toggle
	function toggle(obj) {
		var target = getObject(obj);
		if (!target) return false;

		if (target.type === "window") {
			if (target.dialog("isOpen")) {
				target.dialog('close');
			} else {
				target.dialog('open');
			}
		} else {
			target.opacity = (+!target.opacity);
			if (target.label) {
				target.label.opacity = target.opacity;
			}
		}
	}

	// Element show
	function show(obj) {
		var target = getObject(obj);
		if (!target) return false;

		if (target.type === "window") {
			target.dialog('open');
		} else {
			target.opacity = 1;
			if (target.label) {
				target.label.opacity = target.opacity;
			}
		}
	}

	// Element hide
	function hide(obj) {
		var target = getObject(obj);
		if (!target) return false;

		target.opacity = 0;
		if (target.type === "window") {
			target.dialog('close');
		} else {
			if (target.label) {
				target.label.opacity = target.opacity;
			}
		}
		//target.redraw();
	}

	return {

		// Data setter
		set data (data) {

			// Remove objects
			var cl = oCanvas.canvasList;
			for (var i = 0; i < cl.length; i++) {
				var canvas = cl[i];
				var objects = canvas.draw.objects;
				for (var i in objects) {
					canvas.removeChild(objects[i]);
				}
				canvas.draw.lastObjectID = 0;
				delete cl[i];
			}
			oCanvas.canvasList = [];	
			// Remove windows
			$(thecanvas).empty();
			$('.machine-subwindow').remove();

			// Create object from string
			if (typeof data === 'string') {
				data = eval('('+data+')');
			}
			if (data.nodes)
				scenes[0] = data.nodes;
			else
				return {};

			// create tree
			var nodes = data.nodes;
			for (var i = 0; i < nodes.length; i++) {
				appendNode(nodes[i], thecanvas);
			}
		},

		draw: function() {
/*			var cl = oCanvas.canvasList;
			for (var i = 0; i < cl.length; i++) {
				cl[i].redraw();
			}*/
		},

	};


};


