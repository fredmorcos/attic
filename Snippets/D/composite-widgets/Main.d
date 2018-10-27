module test.Main;

private import gtk.Main, gtk.MainWindow,
				gtk.ScrolledWindow, gtk.HBox,
				gtk.Button, gtk.Label, gtk.VBox,
				gtk.Notebook, gtk.Expander,
				gtk.Widget;

int main (char[][] args) {
	Main.init(args);
	MainWindow win = new MainWindow("test");
	win.setResizable(false);
	win.setSizeRequest(300, 300);
	win.add(new work());
	win.showAll();
	Main.run();
	return 0;
}

class work: Notebook {
	VBox box;
	Button add;
	ScrolledWindow view;
	VBox viewbox;

	this() {
		super();
		box = new VBox(false, 5);
		add = new Button(StockID.ADD, false);
		view = new ScrolledWindow();
		viewbox = new VBox(false, 5);
		view.addWithViewport(viewbox);
		view.setPolicy(PolicyType.AUTOMATIC, PolicyType.AUTOMATIC);
		box.packStart(add, false, false, 0);
		box.packStart(view, true, true, 0);
		appendPage(box, "demarlize");

		add.addOnClicked(&addclicked);
	}

	void addclicked(Button button) {
		viewbox.packStart(new compo(), false, false, 0);
		showAll();
	}
}

class compo: Expander {
	Button del,
		   info;
	Label label;
	HBox box;

	this () {
		super("about marly");
		box = new HBox(false, 2);
		del = new Button(StockID.DELETE, true);
		del.addOnClicked(&deleteclicked);
		info = new Button(StockID.INFO, true);
		label = new Label("marly follaaaa");
		box.packStart(del, false, false, 0);
		box.packStart(label, true, true, 0);
		box.packStart(info, false, false, 0);
		add(box);
	}

	void pack(Widget w) {
		box.packStartDefaults(w);
	}

	void deleteclicked(Button button) {
		destroy();
	}
}

