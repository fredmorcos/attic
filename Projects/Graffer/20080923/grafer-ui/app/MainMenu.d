module app.MainMenu;

private import
	gtk.ImageMenuItem,
	gtk.MenuBar,
	gtk.MenuItem,
	gtk.Menu,
	gtk.SeparatorMenuItem;
	
class MainMenu: MenuBar {
	private:
	
	MenuItem	itemFile,
				itemFileNew_,
				itemFileOpen_,
				itemFileSave_,
				itemFileSaveAs_,
				itemFileExport_,
				itemFileQuit_,
				itemLayout,
				itemLayoutForce_,
				itemHelp,
				itemHelpAbout_;
	Menu		menuFile,
				menuLayout,
				menuHelp;
	
	public:
	
	this () {
		super ();
		
		itemFileNew_ = new ImageMenuItem (StockID.NEW, null);
		itemFileOpen_ = new ImageMenuItem (StockID.OPEN, null);
		itemFileSave_ = new ImageMenuItem (StockID.SAVE, null);
		itemFileSaveAs_ = new ImageMenuItem (StockID.SAVE_AS, null);
		itemFileExport_ = new ImageMenuItem (StockID.CONVERT, null);
		itemFileQuit_ = new ImageMenuItem (StockID.QUIT, null);
		
		with (menuFile = new Menu) {
			append (itemFileNew_);
			append (new SeparatorMenuItem);
			append (itemFileOpen_);
			append (new SeparatorMenuItem);
			append (itemFileSave_);
			append (itemFileSaveAs_);
			append (itemFileExport_);
			append (new SeparatorMenuItem);
			append (itemFileQuit_);
		}
		
		with (itemFile = new MenuItem ("_File")) {
			setSubmenu (menuFile);
		}
		
		itemLayoutForce_ = new MenuItem ("_Force-Based");
		
		with (menuLayout = new Menu) {
			append (itemLayoutForce_);
		}
		
		with (itemLayout = new MenuItem ("_Layout")) {
			setSubmenu (menuLayout);
		}
		
		itemHelpAbout_ = new ImageMenuItem (StockID.ABOUT, null);
		
		with (menuHelp = new Menu) {
			append (itemHelpAbout_);
		}
		
		with (itemHelp = new MenuItem ("_Help")) {
			setSubmenu (menuHelp);
		}
		
		append (itemFile);
		append (itemLayout);
		append (itemHelp);
	}
	
	~this () {
		delete itemFileNew_;
		delete itemFileOpen_;
		delete itemFileSave_;
		delete itemFileSaveAs_;
		delete itemFileExport_;
		delete itemFileQuit_;
		delete itemHelpAbout_;
		delete itemLayoutForce_;
		delete itemFile;
		delete itemLayout;
		delete itemHelp;
		delete menuFile;
		delete menuLayout;
		delete menuHelp;
	}
	
	MenuItem itemFileNew () {
		return itemFileNew_;
	}
	
	MenuItem itemFileOpen () {
		return itemFileOpen_;
	}
	
	MenuItem itemFileSave () {
		return itemFileSave_;
	}
	
	MenuItem itemFileSaveAs () {
		return itemFileSaveAs_;
	}
	
	MenuItem itemFileExport () {
		return itemFileExport_;
	}
	
	MenuItem itemFileQuit () {
		return itemFileQuit_;
	}
	
	MenuItem itemHelpAbout () {
		return itemHelpAbout_;
	}
	
	MenuItem itemLayoutForce () {
		return itemLayoutForce_;
	}
}
