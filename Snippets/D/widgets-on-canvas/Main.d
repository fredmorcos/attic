module Main;
import gtk.Main, gtk.MainWindow, gtk.Entry, gtk.Fixed, gtk.Widget, cairo.Context,
		gtk.EditableIF, gdk.Pixbuf, gdk.Pixmap, cairo.Matrix, tango.math.Math,
		gtk.Button;

int main (char[][] args) {
	Main.init(args);
	
	MainWindow win = new MainWindow ("Widgets on Canvas!");
	win.setSizeRequest(400, 400);
	coolFixed fixed = new coolFixed();
	win.add(fixed);
	win.showAll();
	
	Main.run();
	delete fixed;
	return 0;
}

class coolFixed: Fixed {
	Entry entry;
	CoolButton button;

	this() {
		button = new CoolButton;
		entry = new Entry;
		entry.setText("Hello, World!");
		put(entry, 40, 40);
		put(button, 80, 80);
		entry.addOnExpose(&entryExpose);
		addOnExpose(&fixedExpose);
	}

	~this() {
		delete entry;
	}
	
	bool fixedExpose(GdkEventExpose* event, Widget widget) {
		static double angle = 0.0;

		Context c = new Context(widget.getWindow());
		c.rectangle(widget.getAllocation().x, widget.getAllocation().y, widget.getAllocation().width, widget.getAllocation().height);
		c.clip();
		c.setSourceRgb(0.8, 0.0, 0.0);
		c.setLineWidth(1.0);
		c.rectangle(10, 10, 200, 200);
		c.fill();
		c.setSourceRgb(0.8, 0.8, 0.8);
		c.moveTo(20, 100);
		c.setFontSize(16);
		c.showText(entry.getText());
		
		Pixbuf pb = new Pixbuf(entry.getWindow(), 
			0, 0, 
			entry.getAllocation().width, entry.getAllocation().height);
		c.rotate(angle * PI);
		c.translate(100, 0);
		c.setSourcePixbuf(pb, 40, 65);
		c.paint();

		angle += 0.001;

		if (angle == 2.0)
			angle = 0.0;

//		c.rotate(PI / 4);
//		c.translate(100, -100);
//		c.setSourcePixbuf(pb, 20, 140);
//		c.paint();

		delete pb;
		delete c;
		delete event;
		return false;
	}
	
	bool entryExpose(GdkEventExpose* event, Widget widget) {
		queueDraw();
		return false;
	}
}

class CoolButton: Button {
	this () {
		super ();
		addOnExpose(&expose);
	}

	bool expose (GdkEventExpose* event, Widget widget) {
		return false;
	}
}

