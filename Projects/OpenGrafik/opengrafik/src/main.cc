#include <gtkmm/main.h>

// #include "canvas.h"
#include "ui.h"

int main (int argc, char *argv[])
{	
	Gtk::Main App(argc, argv);
	
	UI x;
	x.run();
	App.run(*(x.getMainWindow()));
	
	return 0;
}
