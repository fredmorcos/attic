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

module ui.MainWindow;

private import 
	GtkMainWindow = gtk.MainWindow,
	gtk.VBox,
	gtk.Widget,
	ui.MainNotebook;

class MainWindow: GtkMainWindow.MainWindow {
protected:
	VBox			mainBox;
	MainNotebook	workspaceNotebook;
	int				documentCount;

public:
	this() {
		super("OpenGrafik");
		setSizeRequest(800, 600);
		setBorderWidth(5);
		
		workspaceNotebook = new MainNotebook;
		
		with (mainBox = new VBox(false, 5)) {
			add(workspaceNotebook);
		}
		
		newDocument;
		newDocument;
		newDocument;
		
		addOnConfigure(&onConfigureEvent);
		
		add(mainBox);
		showAll;
	}
	
	void newDocument () {
		workspaceNotebook.newDocument(
			"Untitled-" ~ tango.text.convert.Integer.toString(++documentCount));
	}
	
	bool onConfigureEvent (GdkEventConfigure *event, Widget widget) {
		workspaceNotebook.configureSize(event.width, event.height);
		return false;
	}
}

