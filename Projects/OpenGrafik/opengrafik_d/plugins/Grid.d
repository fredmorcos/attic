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

module plugins.Grid;

private import
	gfx.View,
	gfx.Geometry,
	gfx.Color,
	cairo.Context;

class Grid: View {
protected:
	int	cellSize = 30,
		blockSize = 120;

	void draw (Context ct) {
		ct.setSourceRgb (fill.red, fill.green, fill.blue);
		ct.rectangle (0, 0, frame.rect.size.width, frame.rect.size.height);
		ct.fill;
		
		ct.setSourceRgb (stroke.red, stroke.green, stroke.blue);
		ct.setLineWidth (0.5);
		
		for (int i = cellSize; i < frame.rect.size.width; i += cellSize) {
			ct.moveTo(i, 0);
			ct.lineTo(i, frame.rect.size.height);
		}

		for (int i = cellSize; i < frame.rect.size.height; i += cellSize) {
			ct.moveTo(0, i);
			ct.lineTo(frame.rect.size.width, i);
		}
		
		ct.stroke;
		
		ct.setLineWidth(1.0);
		
		for (int i = blockSize; i < frame.rect.size.width; i += blockSize) {
			ct.moveTo(i, 0);
			ct.lineTo(i, frame.rect.size.height);
		}

		for (int i = blockSize; i < frame.rect.size.height; i += blockSize) {
			ct.moveTo(0, i);
			ct.lineTo(frame.rect.size.width, i);
		}
		
		ct.stroke;

		tango.io.Stdout.Stdout("hello").newline;
	}

	void onParentVisualChange(View v) {
		size(parent.size);
	}

	void setParent (View p) {
		parent = p;
		parent.onVisualChange.attach(&onParentVisualChange);
	}

public:
	this () {
		super ();
		
		fillColor (Color(1.0, 1.0, 1.0));
		strokeColor (Color(0.6, 0.6, 0.6));

		position(Point (0, 0));
	}
}

