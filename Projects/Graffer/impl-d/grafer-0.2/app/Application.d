module app.Application;

import
	app.InterfaceBuilder,
	app.Workspace,
	gtk.Main,
	gtk.ObjectGtk,
	gtk.MenuItem,
	gtk.Label;

class Application: Object {
private:
	InterfaceBuilder ui;
	
public:	
	this () {
		ui = new InterfaceBuilder;
		
		/* Connect Signals */
		ui.mainMenuFileNew.addOnActivate (&menuNew);
		ui.mainMenuFileClose.addOnActivate (&menuClose);
		ui.mainMenuFileQuit.addOnActivate (&menuQuitApp);
		ui.mainMenuHelpAbout.addOnActivate (&menuShowAbout);
		ui.mainWindow.addOnDestroy (&winQuitApp);
		
		ui.mainWindow.showAll;
	}
	
	void menuClose (MenuItem widget) {
		ui.mainNotebook.removePage (ui.mainNotebook.getCurrentPage);
	}
	
	void menuNew (MenuItem widget) {
		Workspace temp = new Workspace;
		ui.mainNotebook.appendPage (temp, new Label ("New Document"));
	}
	
	void menuShowAbout (MenuItem widget) {
		if (ui.aboutDialog.run == GtkResponseType.GTK_RESPONSE_CANCEL)
			ui.aboutDialog.hide;
	}
	
	void menuQuitApp (MenuItem widget) {
		quitApp;
	}
	
	void quitApp () {
		Main.quit;
	}
	
	void winQuitApp (ObjectGtk widget) {
		quitApp;
	}
}
