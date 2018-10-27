module app.GraferMainWindow;

private import
	gtk.MainWindow,
	gtk.VBox,
	gtk.Image,
	gtk.MenuItem,
	app.MainMenu,
	app.MainNotebook,
	app.GraferAboutDialog;
	
class GraferMainWindow: MainWindow {
	private:
	
	Image			icon;
	MainMenu		mainMenu;
	MainNotebook	mainNotebook;
	VBox			mainBox;
	
	public:
	
	this () {
		super ("Grafer");
		
		icon = new Image ("data/icon.svg");
		mainMenu = new MainMenu;
		mainNotebook = new MainNotebook;
		
		with (mainBox = new VBox (false, 5)) {
			packStart (mainMenu, false, false, 0);
			packStart (mainNotebook, true, true, 0);
		}
		
		/* Connect signals */
		mainMenu.itemHelpAbout.addOnActivate(&showAboutDialog);
		
		maximize;
		setDefaultIcon(icon.getPixbuf);
		setBorderWidth (0);
		add (mainBox);
		showAll;
	}
	
	~this () {
		delete mainMenu;
		delete mainNotebook;
		delete mainBox;
		delete icon;
	}
	
	private:
	
	void showAboutDialog (MenuItem item) {
		static GraferAboutDialog about = null;
		if (!about) {
			about = new GraferAboutDialog;
			about.setLogo (icon.getPixbuf);
		}
		
		if (about.run == GtkResponseType.GTK_RESPONSE_CANCEL)
			about.hide;
	}
}
