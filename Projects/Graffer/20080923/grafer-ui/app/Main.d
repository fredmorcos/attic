module app.Main;

private import
	gtk.Main,
	app.GraferMainWindow;
	
int main (char[][] args) {
	Main.init (args);
	new GraferMainWindow;
	Main.run;
	return 0;
}
