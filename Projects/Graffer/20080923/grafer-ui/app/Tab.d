module app.Tab;

private import
	gtk.HBox,
	gtk.Button,
	gtk.Label,
	gtk.Image;

class Tab: HBox {
	private:
	
	Label	label;
	Button	closeButton_;
	
	public:
	
	this () {
		this ("New Document");
	}
	
	this (char[] text) {
		super (false, 5);
		
		label = new Label (text);
		
		with (closeButton_ = new Button ()) {
			Image img = new Image (StockID.CLOSE, GtkIconSize.MENU);
			img.setSizeRequest (10, 10);
			setImage (img);
			setRelief (GtkReliefStyle.NONE);
			setSizeRequest (22, 22);
		}		
		
		packStart (label, true, true, 0);
		packStart (closeButton_, false, false, 0);
		
		showAll;
	}
	
	~this () {
		delete label;
		delete closeButton_;
	}
	
	void setText (char[] text) {
		label.setText (text);
	}
	
	Button closeButton () {
		return closeButton_;
	}
}
