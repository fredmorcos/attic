module app.MainNotebook;

private import
	gtk.Notebook,
	gtk.Widget,
	app.Tab;

class MainNotebook: Notebook {
	public:
	
	this () {
		super ();
	}
	
	~this () {
	}
	
	void append (Widget widget, char[] text) {
		appendPage (widget, new Tab (text));
	}
}
