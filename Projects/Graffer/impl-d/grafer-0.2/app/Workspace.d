module app.Workspace;

import
	gtk.ScrolledWindow,
	gtk.DrawingArea;

class Workspace: ScrolledWindow {
public:
	DrawingArea		area;
	
	this () {
		super ();
		
		area = new DrawingArea;
		addWithViewport (area);
		showAll;
	}
}
