module app.InterfaceBuilder;

import
	gtk.Widget,
	gtk.Window,
	gtk.ImageMenuItem,
	gtk.AboutDialog,
	gtk.Notebook,
	gtk.HBox,
	gtk.VBox,
	gtk.DrawingArea,
	gtk.ScrolledWindow,
	gtk.ComboBox,
	glade.Glade,
	tango.io.FilePath;
	
class InterfaceBuilder: Object {
private:
	Glade glade;
	
public:
	Window mainWindow;
	AboutDialog aboutDialog;
	Notebook mainNotebook;
	ImageMenuItem
		mainMenuFileQuit,
		mainMenuFileNew,
		mainMenuFileClose,
		mainMenuHelpAbout;
	ComboBox shapesCombo;
	VBox shapesBox;
	
	this () {
		with (glade = new Glade ("data/ui.glade")) {
			mainWindow = cast(Window) getWidget ("mainWindow");
			mainMenuFileNew = cast(ImageMenuItem) getWidget ("mainMenuFileNew");
			mainMenuFileClose = cast(ImageMenuItem) getWidget ("mainMenuFileClose");
			mainMenuFileQuit = cast(ImageMenuItem) getWidget ("mainMenuFileQuit");
			mainMenuHelpAbout = cast(ImageMenuItem) getWidget ("mainMenuHelpAbout");
			mainNotebook = cast(Notebook) getWidget ("mainNotebook");
			aboutDialog = cast(AboutDialog) getWidget ("aboutDialog");
			shapesBox = cast(VBox) getWidget ("shapesBox");
			shapesCombo = new ComboBox;
			reloadShapesCombo;
			shapesBox.packStart (shapesCombo, false, false, 0);
			shapesBox.showAll;
		}
		
		delete glade;
	}
	
	void reloadShapesCombo () {
		foreach (i; (new FilePath ("data/shapes")).toList)
			shapesCombo.appendText (i.name);
		shapesCombo.setActive (0);
	}
}
