module app.GraferAboutDialog;

private import
	gtk.AboutDialog,
	gtk.Image;
	
class GraferAboutDialog: AboutDialog {
	private:
	
	char[][]	authors,
				documenters,
				artists;
	
	public:
	
	this () {
		super ();
		
		authors ~= "Frederic Morcos <fred.morcos@gmail.com>";
		documenters ~= "Frederic Morcos <fred.morcos@gmail.com>";
		artists ~= "Frederic Morcos <fred.morcos@gmail.com>";
	
		setSkipTaskbarHint (true);
		
		setProgramName ("Grafer");
		setVersion ("0.1");
		setCopyright ("Copyright (C) 2008	Frederic-Gerald Morcos");
		setAuthors (authors);
		setDocumenters (documenters);
		setArtists (artists);
		setLicense ("Licensed under the GPLv3");
		setWebsite ("http://grafer.googlecode.com");
	}
	
	~this () {
		delete authors;
		delete documenters;
		delete artists;
	}
}
