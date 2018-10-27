#include <gtkmm.h>
#include "canvas.h"

int main (int argc, char *argv[])
{
	Gtk::Main CanvasDemo(argc, argv);

	Gtk::Window window(Gtk::WINDOW_TOPLEVEL);
	Gtk::VBox vbox;
	Gtk::HBox hbox;
	Gtk::Button aboutButton(Gtk::Stock::ABOUT);
	Gtk::Button quitButton(Gtk::Stock::QUIT);
	DemoCanvas canvas;

	hbox.pack_start(vbox, false, true);
	hbox.pack_start(canvas, true, true);

	vbox.pack_start(aboutButton, true, true);
	vbox.pack_start(quitButton, true, true);

	window.add(hbox);
	window.maximize();
	window.show_all();

	Gtk::Main::run(window);
	return 0;
}
