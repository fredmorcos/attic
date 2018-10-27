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

module gfx.SelectionRectangle;

private import
	gtk.Widget,
	cairo.Context,
	core.BaseObject;

class SelectionRectangle: BaseObject {
protected:
	bool onExpose (GdkEventExpose* event, Widget widget) {
		GtkAllocation allocation = getAllocation;
		context = new Context(getWindow);
//		context.rectangle(event.area.x, event.area.y, 
//						  event.area.width, event.area.height);
//		context.clip;
		
//		context.setSourceRgba(1.0, 1.0, 1.0, 0.0);
//		context.setOperator(CairoOperator.SOURCE);
//		context.paint;
		
		context.setSourceRgba(0.2, 0.2, 0.8, 0.5);
		context.setLineWidth(4.0);
		context.rectangle(0, 0, allocation.width, allocation.height);
		context.fillPreserve;
		context.stroke;
		
		return true;
	}
}

