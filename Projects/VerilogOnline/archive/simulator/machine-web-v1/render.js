function Machine(thecanvas) {

	var objects = {};

	var canvasList = [];
	var windowList = [];

	var scenes = [];
	var _step = 0;

	var register = function(settings, core) {
		
		return oCanvas.extend({
			core: core,

			shapeType: "rectangular",
			strokeWidth: 1,
			strokeColor: "black",

			_: oCanvas.extend({}, core.displayObject._, {
				title: {},
				value: {},
				strokeColor: "black",
				strokeWidth: 1
			}),

			init: function() {
				var defaults = {
					fill: this.strokeColor,
					x: this.width / 2,
					y: this.height / 2,
					align: "center",
					baseline: "middle",
					text: ""
				};

				// Init title
				this._.title = core.display.text(oCanvas.extend({}, defaults, this._.title));
				var obj = this._.title;
				this.addChild(obj);
				obj.y -= obj.height * obj.lineHeight / 2;

				// Init value
				this._.value = core.display.text(oCanvas.extend({}, defaults, this._.value));
				var obj = this._.value;
				this.addChild(obj);
				obj.y += obj.height * obj.lineHeight / 2;
			},

			get title () {
				return this._.title;
			},

			get value () {
				return this._.value;
			},

			set title (text) {
				var obj = this._.title;
				if (typeof text == "object") {
					oCanvas.extend(obj, text);
				} else {
					obj.text = text;
				}
			},

			set value (text) {
				var obj = this._.value;
				if (typeof text == "object") {
					oCanvas.extend(obj, text);
				} else {
					obj.text = text;
				}
			},

			draw: function() {
                var canvas = this.core.canvas,
				    origin = this.getOrigin(),
				    x = this.abs_x - origin.x,
				    y = this.abs_y - origin.y;

				canvas.beginPath();
				
				canvas.lineWidth = this.strokeWidth;
				canvas.strokeStyle = this.strokeColor;

				canvas.strokeRect(x, y, this.width, this.height);
				canvas.closePath();

				if (this.fill !== "") {
					canvas.fillStyle = this.fill;
					canvas.fill();
				}


				return this;

			},


		}, settings);

	};

	oCanvas.registerDisplayObject("register", register, "init");

	var memory = function(settings, core) {

		return oCanvas.extend({
			core: core,

			shapeType: "rectangular",

			_: oCanvas.extend({}, core.displayObject._, {
				title: {},
				value: {},
				strokeColor: "blue",
				strokeWidth: 1,
				borderRadius: 10,
				width: 200,
				height: 100
			}),

			init: function() {
				var defaults = {
					fill: this.strokeColor,
					x: this.width / 2,
					y: this.height / 2,
					align: "center",
					baseline: "middle",
					text: ""
				};

				// Init title
				this._.title = core.display.text(oCanvas.extend({}, defaults, this._.title));
				var obj = this._.title;
				this.addChild(obj);
			},

			get title () {
				return this._.title;
			},

			set title (text) {
				var obj = this._.title;
				if (typeof text == "object") {
					oCanvas.extend(obj, text);
				} else {
					obj.text = text;
				}
			},

			draw: function() {
                var canvas = this.core.canvas,
				    origin = this.getOrigin(),
				    x = this.abs_x - origin.x,
				    y = this.abs_y - origin.y,
					r = this._.borderRadius;

				canvas.beginPath();
				
				canvas.lineWidth = this.strokeWidth;
				canvas.strokeStyle = this.strokeColor;

				canvas.moveTo(r, 0);
				canvas.lineTo(this.width - r, 0);
				canvas.arc(this.width - r, r, r, 3 * Math.PI / 2, 0, false)
				canvas.lineTo(this.width, this.height - r);
				canvas.arc(this.width - r, this.height - r, r, 0, Math.PI / 2, false)
				canvas.lineTo(r, this.height);
				canvas.arc(r, this.height - r, r, Math.PI / 2, Math.PI, false)
				canvas.lineTo(0, r);
				canvas.arc(r, r, r, Math.PI, 3 * Math.PI / 2, false)
				canvas.stroke();

				canvas.closePath();

				if (this.fill !== "") {
					canvas.fillStyle = this.fill;
					canvas.fill();
				}


				return this;

			},

		}, settings);
	};

	oCanvas.registerDisplayObject("memory", memory, "init");

	var constant = function(settings, core) {

		return oCanvas.extend({
			core: core,

			shapeType: "rectangular",

			_: oCanvas.extend({}, core.displayObject._, {
				title: {},
				value: {},
				strokeColor: "black",
				strokeWidth: 1,
				width: 100,
				height: 86.603
			}),

			init: function() {
				this.height = 0.86603 * this.width;

				var defaults = {
					fill: this.strokeColor,
					x: this.width / 2,
					y: this.height / 2,
					align: "center",
					baseline: "bottom",
					text: ""
				};

				// Init title
				this._.title = core.display.text(oCanvas.extend({}, defaults, this._.title));
				var obj = this._.title;
				this.addChild(obj);

			},

			get title () {
				return this._.title;
			},

			set title (text) {
				var obj = this._.title;
				if (typeof text == "object") {
					oCanvas.extend(obj, text);
				} else {
					obj.text = text;
				}
			},

			draw: function() {
                var canvas = this.core.canvas,
				    origin = this.getOrigin(),
				    x = this.abs_x - origin.x,
				    y = this.abs_y - origin.y;

				var w1 = (this.width - this.smallWidth) / 2;

				canvas.beginPath();
				
				canvas.lineWidth = this.strokeWidth;
				canvas.strokeStyle = this.strokeColor;

				canvas.moveTo(0, 0);
				canvas.lineTo(this.width, 0);
				canvas.lineTo(this.width / 2, this.height);
				canvas.lineTo(0, 0);
				canvas.stroke();

				canvas.closePath();

				if (this.fill !== "") {
					canvas.fillStyle = this.fill;
					canvas.fill();
				}


				return this;

			},

		}, settings);
	};

	oCanvas.registerDisplayObject("constant", constant, "init");

	var mux = function(settings, core) {

		return oCanvas.extend({
			core: core,

			shapeType: "rectangular",

			_: oCanvas.extend({}, core.displayObject._, {
				title: {},
				value: {},
				strokeColor: "black",
				strokeWidth: 1,
			}),

			init: function() {
				var defaults = {
					fill: this.strokeColor,
					x: this.width / 2,
					y: this.height / 2,
					align: "center",
					baseline: "middle",
					text: ""
				};

				// Init title
				this._.title = core.display.text(oCanvas.extend({}, defaults, this._.title));
				var obj = this._.title;
				this.addChild(obj);

				this.smallWidth = this.width - 2 * 0.5773 * this.height;
			},

			get title () {
				return this._.title;
			},

			set title (text) {
				var obj = this._.title;
				if (typeof text == "object") {
					oCanvas.extend(obj, text);
				} else {
					obj.text = text;
				}
			},

			draw: function() {
                var canvas = this.core.canvas,
				    origin = this.getOrigin(),
				    x = this.abs_x - origin.x,
				    y = this.abs_y - origin.y;

				var w1 = (this.width - this.smallWidth) / 2;

				canvas.beginPath();
				
				canvas.lineWidth = this.strokeWidth;
				canvas.strokeStyle = this.strokeColor;

				canvas.moveTo(0, 0);
				canvas.lineTo(this.width, 0);
				canvas.lineTo(this.width - w1, this.height);
				canvas.lineTo(w1, this.height);
				canvas.lineTo(0, 0);
				canvas.stroke();

				canvas.closePath();

				if (this.fill !== "") {
					canvas.fillStyle = this.fill;
					canvas.fill();
				}


				return this;

			},

		}, settings);
	};

	oCanvas.registerDisplayObject("mux", mux, "init");

	var polyline = function(settings, core) {

		//return oCanvas.extend(new labeledObject(settings, core), {
		return oCanvas.extend({
			core: core,

			shapeType: "rectangular",
			
			init: function() {
				if (!this.arrows) this.arrows = {};
				this.setGravity();
				
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
				if (inside) {

					// Check point in polygon
					//     http://alienryderflex.com/polygon/
					var px = this.core.pointer.x, py = this.core.pointer.y;
					var i, sides = this.points.length, j = sides - 1;
					var odd = false;
					for (i = 0; i < sides; i++) {
						if (this.points[i][1] < py && this.points[j][1] >= py
								|| this.points[j][1] < py && this.points[i][1] >= py) {
							if (this.points[i][0] + (py - this.points[i][1]) / 
									(this.points[j][1] - this.points[i][1]) * this.points[j][0] - this.points[i][0] < px) {
								odd = !odd;
							}
						}
						j = i;
					}
					return !odd;
				}
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

				// Arrows
				if (this.arrows.start) {
					this.drawArrow('start');
				}
				if (this.arrows.end) {
					this.drawArrow('end');
				}

			},

			drawArrow: function(pos) {
				var canvas = this.core.canvas;
				var size = this.arrows.size || 16;
				var xPos, yPos;
				canvas.save()
				if (pos === "start") {
					var dsx = this.points[1][0] - this.points[0][0];
					var dsy = this.points[1][1] - this.points[0][1];
					xPos = this.points[0][0] - this.x;
					yPos = this.points[0][1] - this.y;
					if (this.arrows.start.size) size = this.arrows.start.size;

				} else if (pos === "end") {
					var last = this.points.length - 1;
					var dsx = this.points[last-1][0] - this.points[last][0];
					var dsy = this.points[last-1][1] - this.points[last][1];
					xPos = this.points[last][0] - this.x;
					yPos = this.points[last][1] - this.y;
					if (this.arrows.end.size) size = this.arrows.end.size;
				}
				var angle = Math.atan2(dsy, dsx);

				canvas.translate(xPos, yPos);
				canvas.rotate(angle);

				// Draw
				var xLeg = size*0.96, yLeg = size*0.25;
				canvas.beginPath();
				canvas.lineTo(xLeg, yLeg);
				canvas.lineTo(xLeg, -yLeg);
				canvas.lineTo(0, 0);
				canvas.fillStyle = this.strokeColor;
				canvas.stroke();
				canvas.closePath();
				canvas.fill();
				canvas.restore();
			},

		}, settings);
	};


	oCanvas.registerDisplayObject("polyline", polyline, "init");


	/**
	 * Extract properties from object if availible 
	 *
	 * @param keys {Object} Properties to extract
	 * @param src {Object} Source 
	 * @return {Object} New object 
	 */
	function extract(keys, src) {
		var ret = new Object();
		for (var key in keys) {
			if (src.hasOwnProperty(key)) {
				if (typeof src[key] == 'Object') {
					ret[key] = new Object(src[key]);
				} else {
					ret[key] = src[key];
				}
			}
		}
		return ret;
	}

	function addLabel(obj, label) {

		if (typeof obj !== "object") return false;

		var core = obj.core;
		var canvas = core.canvas;

		if (typeof obj.draw === "function" && typeof obj.drawObject === "undefined") {
			obj.drawObject = obj.draw;

			obj.draw = function() {
				obj.drawObject();

				canvas.save();
				canvas.translate(-obj.x, -obj.y);
				obj.label.draw();
				canvas.restore();
			};
		}

		if (typeof obj.isPointerInside === "function" && typeof obj.isPointerInsideObject === "undefined") {
			obj.isPointerInsideObject = obj.isPointerInside;

			obj.isPointerInside = function() {
				if (obj.isPointerInsideObject()) {
					return true;
				}
				
				if (obj.label) {
					return core.tools.isPointerInside(obj.label, core.pointer);
				}
				return false;
			};
		}

		obj.__defineSetter__("label", function(label) {
			var settings = {
				fill: this.strokeColor || "black",
				x: this.width / 2,
				y: this.height / 2,
				align: "center",
				baseline: "middle",
				text: ""
			};
			if (typeof label == "object") {
				oCanvas.extend(settings, label);
			} else {
				settings.text = label;
			}
			var lines = settings.text.toString().split("\n");
			var obj = core.display.text(settings);
			obj.parent = this;

			// Call setter to set abs_x, middle multiline labels
			obj.x += 0;
			obj.y -= (lines.length - 1) * obj.height * obj.lineHeight / 2;
			this._.label = obj;
			obj.redraw();
		});

		obj.__defineGetter__("label", function() {
			return this._.label;
		});

		obj.label = label;

	}

	// Create an html node
	function appendNode(data, parent_node) {
		switch (data.type) {
		case "window":
			var id = data.id;
			if (!id) 
				id = 'dialog-' + (new Date()).getTime();
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
			if (!id) id = 'canvas-' + (new Date()).getTime();
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
			return obj;
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
		
		var ext_elem = false;
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
			addLabel(obj, data.label);
			break;

		case "line":
			var obj = parent_node.display.line(oCanvas.extend(defaults, data));
			break;

		case "text": 
			var obj = parent_node.display.text(data);
			parent_node.addChild(obj);
			return obj;

		case "register": 
			var obj = parent_node.display.register(data);
			parent_node.addChild(obj);
			return obj;

		case "memory": 
			var obj = parent_node.display.memory(data);
			parent_node.addChild(obj);
			return obj;

		case "mux": 
		case "operator": 
			var obj = parent_node.display.mux(data);
			parent_node.addChild(obj);
			return obj;

		case "constant": 
			var obj = parent_node.display.constant(data);
			parent_node.addChild(obj);
			return obj;

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
			scenes[0][data.id] = oCanvas.extend({}, parent_node.displayObject._, defaults, data);
			delete scenes[0][data.id].stroke; 	// FIXME
			delete scenes[0][data.id].x;
			delete scenes[0][data.id].y;
			delete scenes[0][data.id].abs_x;
			delete scenes[0][data.id].abs_y;
			delete scenes[0][data.id].width;
			delete scenes[0][data.id].height;
		}

		
		// check for labels
		var label = null;
		if (data.label && !ext_elem) {
			addLabel(obj, data.label);
		}

/*		if (data.label && obj.type != "polyline" && false) {
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

		}*/
		
		// Add events
		var hlevent = false;
		if (data.click && !ext_elem) { 	// XXX
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
		}


		return obj;
	};

	function addState(scene) {
		var ref = scene.reference;
		var objs = scene.objects;
		if (ref == "first") {
			scenes.push(objs);
		} else if (ref == "previous") {
			var last = scene[scene.lenght-1];
			obj = oCanvas.extend({}, last, objs);
			scenes.push(objs);
		} else {
			log("Error adding scenen. Unknown reference " + ref);
		}

	}

	function updateStates() { 

		for (var i in objects) {
			var obj = objects[i];
			if (scenes[0][i]) {
				obj = oCanvas.extend(obj, scenes[0][i]);
				obj.redraw(); // FIXME use canvas
			}
		}
		if (_step > 0) {
			var cur = scenes[_step];
			for (var i in cur) {
				var obj = objects[i];
				if (obj !== "undefined") {
					obj = oCanvas.extend(obj, cur[i]);
					obj.redraw(); // FIXME use canvas
				}
			}
		}
	}


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
				var obj = canvas.draw.objects;
				for (var i in obj) {
					canvas.removeChild(obj[i]);
				}
				canvas.draw.lastObjectID = 0;
				delete cl[i];
			}
			oCanvas.canvasList = [];	
			// Remove windows
			$(thecanvas).empty();
			$('.machine-subwindow').remove();

			// Remove scenes
			for (var i in scenes) {
				delete scenes[i];
			}
			scenes = [];

			// Create object from string
			if (typeof data === 'string') {
				data = eval('('+data+')');
			}


			// Parse element 0
			/*
			if (!(data[0] instanceof Array)) {
				log("Invalid format");
				return false;
			}
			*/
			if (data.objects)
				scenes[0] = data.objects;
			else
				return {};

			// Add frames
			for (var i = 0; i < data.states.length; i++) {
				addState(data.states[i]);
			}

			// create tree
			var nodes = data.objects;
			for (var i = 0; i < nodes.length; i++) {
				appendNode(nodes[i], thecanvas);
			}

		},

		get obj() {
			return objects;
		},

		get step () {
			return _step;
		},

		set step (step) {
			var steps = scenes.length - 1;
			if (step < 0) step = 0;
			if (step > steps) step = steps;
			_step = step;
			updateStates();
		},


		get steps() {
			return scenes.length - 1;
		},

		stepInfo: function() {
			var steps = scenes.length - 1;
			return _step + " of " + steps;
		},

		draw: function() {
/*			var cl = oCanvas.canvasList;
			for (var i = 0; i < cl.length; i++) {
				cl[i].redraw();
			}*/
		},

	};


};


