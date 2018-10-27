using GLib;
using Gtk;

public class Main : Object  {
	public Main () {
		Window window = new Window();
		window.set_title("Hello World");
		window.show_all();
		window.destroy.connect(on_destroy);
	}

	public void on_destroy (Widget window)
	{
		Gtk.main_quit();
	}

	static int main (string[] args)
	{
		Gtk.init(ref args);
		var app = new Main();
		Gtk.main();
		return 0;
	}
}
