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

module ui.DocumentViewport;

private import
	gtk.ScrolledWindow,
	ui.DiagramCanvas;
	
class DocumentViewport: ScrolledWindow {
protected:
	DiagramCanvas canvas;
	
	bool onScroll (GtkScrollType type, int arg2, ScrolledWindow window) {
		canvas.queueDraw;
		return true;
	}
	
public:
	this () {
		super(PolicyType.ALWAYS, PolicyType.ALWAYS);
		
		canvas = new DiagramCanvas;
		addWithViewport(canvas);
		
		addOnScrollChild(&onScroll);
		
		showAll;
	}
	
	void configureSize (int width, int height) {
		canvas.configureSize(width, height);
	}
}

