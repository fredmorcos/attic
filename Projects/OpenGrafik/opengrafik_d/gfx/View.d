/*
 *	This file is part of OpenGrafik.
 *
 *	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>
 *
 *	OpenGrafik is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	OpenGrafik is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OpenGrafik.  If not, see <http://www.gnu.org/licenses/>.
 */

module gfx.View;

private import
	gfx.Geometry,
	gfx.Color,
	cairo.Context,
	tango.core.Signal;

class View {
protected:
	View	parent;
	View[]	subviews;
	Frame*	frame;
	Color	fill, stroke;

	void draw (Context ct) {
	}
	
public:
	Signal!(View) onVisualChange;

	this () {
		frame = new Frame;
		opacity (1.0);
	}
	
	void addSubview (View v) {
		foreach (View sv; subviews)
			if (sv == v)
				return;
		subviews ~= v;
		v.setParent (this);
		
	}

	void render (Context ct) {
		Point pos = absolutePosition;
		
		ct.save;
		
		ct.rectangle (pos.x, pos.y, frame.rect.size.width, frame.rect.size.height);
		ct.clip;
		ct.translate (pos.x, pos.y);
		ct.rotate (absoluteRotation);
		
		draw (ct);
		
		foreach (View v; subviews)
			v.render (ct);
		
		ct.restore;
	}
	
	void rotation (int rot) {
		frame.rotation = rot;
		onVisualChange(this);
	}
	
	int rotation () {
		return frame.rotation;
	}
	
	void opacity (double op) {
		frame.opacity = op;
		onVisualChange(this);
	}
	
	double opacity () {
		return frame.opacity;
	}
	
	void position (Point pos) {
		frame.rect.position = pos;
		onVisualChange(this);
	}
	
	Point position () {
		return frame.rect.position;
	}
	
	void size (Size s) {
		frame.rect.size = s;
		onVisualChange(this);
	}
	
	Size size () {
		return frame.rect.size;
	}
	
	void fillColor (Color col) {
		fill = col;
		onVisualChange(this);
	}
	
	Color fillColor () {
		return fill;
	}
	
	void strokeColor (Color col) {
		stroke = col;
		onVisualChange(this);
	}
	
	Color strokeColor () {
		return stroke;
	}
	
	void setParent (View p) {
		parent = p;
	}

	View getParent () {
		return parent;
	}
	
	int absoluteRotation () {
		if (parent !is null)
			return parent.absoluteRotation + rotation;
		return rotation;
	}
	
	Point absolutePosition () {
		if (parent !is null)
			return parent.absolutePosition + frame.rect.position;
		return frame.rect.position;
	}
}

