using GLib;
using Gtk;

public class gtkSample : Window
{
	construct
	{
		title = "Sample Gtk Window";
		create_widgets ();
	}
	
	public void create_widgets ()
	{
		destroy += Gtk.main_quit;
		var button = new Button.with_label ("Hello World");
		button.clicked += btn =>
		{
			title = btn.label;
		};
		
		add (button);
	}
	
	static int main (string [] args)
	{
		Gtk.init (ref args);
		var sample = new gtkSample ();
		sample.show_all ();
		Gtk.main ();
		return 0;
	}
}
