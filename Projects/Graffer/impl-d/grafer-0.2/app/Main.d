module app.Main;

import
	gtk.Main,
	app.Application,
	lib.temp,
	tango.io.Stdout;
	
int main (char[][] args) {
	Hello there = new Hello;
	Stdout(there.hello).newline;
	Main.init (args);
	new Application;
	Main.run;
	return 0;
}
