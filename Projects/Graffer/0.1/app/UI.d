/*
	This file is part of Grafer.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	Grafer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Grafer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Grafer.  If not, see <http://www.gnu.org/licenses/>.
*/

module app.UI;

private import
	graph.adt.Graph,
	graph.alg.ForceLayout,
   	gtk.MainWindow,
	gtk.HBox,
	gtk.VBox,
	gtk.DrawingArea,
	gtk.ScrolledWindow,
	gtk.Notebook,
	gtk.AboutDialog,
	gtk.Button,
	gtk.Image,
	gtk.Label,
	app.UIGraph,
	app.UILayout,
	app.UIFile;

class UI: MainWindow {
protected:
	Notebook		workspace;
	HBox			mainBox;
	VBox			toolBox;
	DrawingArea		canvas;
	ScrolledWindow	canvasViewport;
	UIGraph			graphUI;
	UILayout		layoutUI;
	UIFile			fileUI;
	AboutDialog		about;
	Button			aboutButton;
	Image			icon;

public:	
	this() {
		super("Grafer");
		setBorderWidth(5);
		maximize();
		
		icon = new Image("data/icon.svg");
		setDefaultIcon(icon.getPixbuf());

		aboutButton = new Button(StockID.ABOUT);
		aboutButton.addOnClicked(&aboutButtonClicked);
		
		graphUI = new UIGraph();
		layoutUI = new UILayout();
		fileUI = new UIFile();
		canvas = new DrawingArea();

		with (canvasViewport = new ScrolledWindow(canvas)) {
			setPolicy(PolicyType.AUTOMATIC, PolicyType.AUTOMATIC);
		}
		
		with (workspace = new Notebook()) {
			appendPage(canvasViewport, "Graph1");
		}
		
		with (toolBox = new VBox(false, 5)) {
			packStart(graphUI, false, false, 0);
			packStart(layoutUI, false, false, 0);
			packStart(fileUI, false, false, 0);
			packStart(new Label(""), true, true, 0);
			packStart(aboutButton, false, false, 0);
		}

		with (mainBox = new HBox(false, 5)) {
			packStart(toolBox, false, false, 0);
			packStart(workspace, true, true, 0);
		}

		add(mainBox);
		showAll();

		canvas.queueResize();
	}
	
	void aboutButtonClicked(Button button) {
		char[][]	authors,
					documenters,
					artists;
		
		authors ~= "Frederic Morcos <fred.morcos@gmail.com>";
		documenters ~= "Frederic Morcos <fred.morcos@gmail.com>";
		artists ~= "Frederic Morcos <fred.morcos@gmail.com>";
		
		about = new AboutDialog();
		about.setProgramName("Grafer");
		about.setVersion("0.1");
		about.setCopyright("Copyright (C) 2008	Frederic-Gerald Morcos");
		about.setLogo(icon.getPixbuf());
		about.setAuthors(authors);
		about.setDocumenters(documenters);
		about.setArtists(artists);
		about.setLicense("Licensed under the GPLv3");
		about.setWebsite("http://grafer.googlecode.com");
		
		if (about.run() == GtkResponseType.GTK_RESPONSE_CANCEL)
			about.destroy();
		
		delete authors;
		delete documenters;
		delete artists;
		delete about;
	}

	~this() {
		delete graphUI;
		delete layoutUI;
		delete fileUI;
		delete canvas;
		delete toolBox;
		delete mainBox;
		delete icon;
		delete aboutButton;
	}
}

