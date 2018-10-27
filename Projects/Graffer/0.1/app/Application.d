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

module app.Application;

private import
	app.UI, 
	graph.adt.Graph, 
	graph.adt.Vertex, 
	graph.adt.Quadrant,
	graph.alg.ForceLayout, 
	graph.alg.SimpleForceLayout, 
	graph.alg.TimeForceLayout,
	graph.alg.BHForceLayout,
	graph.alg.Shape,
	graph.xml.GraphFile,
	tango.io.Stdout, 
	tango.core.Memory,
	tango.time.StopWatch,
	gtk.Main, 
	gtk.Button, 
	gtk.Widget, 
	gtk.ToggleButton,
	gtk.FileChooserDialog,
	gdk.Drawable,
	cairo.Context, 
	cairo.Surface, 
	cairo.ImageSurface;

class Application: UI {
protected:
	Graph			*graph;
	ForceLayout		layout;
	bool			stop = false,
					animate = false;
	Vertex*			dragged,
					selected;
	Context			context;

public:
	this() {
		super();
		
		graph = new Graph();

		graphUI.getEdgesButton().addOnClicked(&edgesButtonClick);
		graphUI.getVerticesButton().addOnClicked(&verticesButtonClick);
		layoutUI.getRunButton.addOnClicked(&runButtonClick);
		layoutUI.getStopButton.addOnClicked(&stopButtonClick);
		layoutUI.getWallCheck().addOnToggled(&wallCheckToggle);
		layoutUI.getAnimateCheck().addOnToggled(&animateCheckToggle);
		fileUI.getExportButton().addOnClicked(&exportButtonClick);
		fileUI.getSaveButton().addOnClicked(&saveButtonClick);
		fileUI.getOpenButton().addOnClicked(&openButtonClick);
		fileUI.getQuitButton().addOnClicked(&quitButtonClick);
		canvas.addOnExpose(&canvasExpose);
		canvas.addOnConfigure(&canvasConfigure);
		canvas.addOnButtonPress(&canvasButtonPress);
		canvas.addOnButtonRelease(&canvasButtonRelease);
		canvas.addOnMotionNotify(&canvasMotionNotify);
	}

	~this() {
		delete context;
		delete graph;
	}

private:
	void animateCheckToggle(ToggleButton button) {
		animate = layoutUI.getEnableAnimation();
	}

	void wallCheckToggle(ToggleButton button) {
		graph.wall = layoutUI.getEnableWall();
		canvas.queueDraw();
	}

	bool canvasConfigure(GdkEventConfigure *event, Widget widget) {
		graph.width = event.width;
		graph.height = event.height;

		return true;
	}

	bool canvasButtonPress(GdkEventButton *event, Widget widget) {
		Vertex* v = graph.getVertexByPosition(event.x, event.y);
		
		if (!v) return false;

		if (event.button == 3) {
			v.locked = !v.locked;
			canvas.queueDraw();
		}
		else if (event.button == 1)
			dragged = v;

		return false;
	}
	
	bool canvasButtonRelease(GdkEventButton *event, Widget widget) {
		dragged = null;
		return false;
	}

	bool canvasMotionNotify(GdkEventMotion *event, Widget widget) {
		if (dragged) {
			dragged.pos.x = event.x;
			dragged.pos.y = event.y;
			canvas.queueDraw();
		}
/*		else {
			if (selected)
				selected.selected = false;

			selected = graph.getVertexByPosition(event.x, event.y);
			
			if (selected)
				selected.selected = true;
		}
		canvas.queueDraw();
*/
		return false;
	}

	void stopButtonClick(Button button) {
		stop = true;
	}

	void runButtonClick(Button button) {
		double		time = 0.0,
					limit = 0.0;
		StopWatch	timer;

		switch (layoutUI.getAlgorithmType()) {
			case ForceLayoutType.TIME:
				layout = new TimeForceLayout();
				break;
			case ForceLayoutType.BH:
				layout = new BHForceLayout();
				break;
			default:
				layout = new SimpleForceLayout();
		}

		layout.eConst = layoutUI.getElectricConst();
		layout.sConst = layoutUI.getSpringConst();
		layout.sLength = layoutUI.getSpringNL();
		layout.sNL = layoutUI.getEnableNL();
		layout.damping = cast(double) (layoutUI.getDamping() / 10.0);
		layout.timestep = cast(double) (layoutUI.getTimestep() / 100.0);
		layout.graph = graph;

		limit = graph.vertices.length + 1;

		layoutUI.getStopButton().setSensitive(true);
		layoutUI.getRunButton().setSensitive(false);

//		GC.disable();
		timer.start();
		while (stop == false) {
			layout.run();

			if (layout.energy.x < limit && layout.energy.y < limit)
				break;

			if (animate) {
				canvas.queueDraw;
//				while(Main.eventsPending)
				Main.iterationDo(false);
			}
		}
		time = timer.stop();
//		GC.enable();

		centerGraph();
		canvas.queueDraw();
		while(Main.eventsPending()) Main.iterationDo(false);

		layout.printInfo();
		Stdout.formatln("Time: {}", time).newline;

		layoutUI.getStopButton().setSensitive(false);
		layoutUI.getRunButton().setSensitive(true);
		stop = false;

		delete layout;
	}
	
	void centerGraph() {
		Quadrant	q;
		graph.getQuadrant(q);
		canvas.setSizeRequest(q.length, q.length);
		graph.shiftVerticesBy(-q.x, -q.y);
	}

	bool canvasExpose(GdkEventExpose *event, Widget widget) {
		context = new Context(widget.getWindow());
//		context.setTolerance(5.0);
//		context.setAntialias(CairoAntialias.NONE);
		context.setSourceRgb(0.2, 0.2, 0.2);
		context.setLineWidth(1.0);
		context.rectangle(event.area.x, event.area.y, 
							event.area.width, event.area.height);
		context.clip();
		graph.expose(context);
		return true;
	}

	void quitButtonClick(Button button) {
		delete this;
		Main.exit(0);
	}

	void edgesButtonClick(Button button) {
		if (!graph.vertices) return;
		createEdges(graph, graphUI.getGraphShape());
		canvas.queueDraw();
	}

	void verticesButtonClick(Button button) {
		randomVertices(graph, graphUI.getNumberOfVertices());
		createEdges(graph, graphUI.getGraphShape());
		canvas.queueDraw();
	}

	void exportButtonClick(Button button) {
		if (!graph.vertices)
			return;
		
		char[][] texts;
		texts ~= "OK";
		texts ~= "Cancel";
		ResponseType[] responses;
		responses ~= ResponseType.GTK_RESPONSE_OK;
		responses ~= ResponseType.GTK_RESPONSE_CANCEL;
		FileChooserDialog	dialog = new FileChooserDialog("Export", 
										this, FileChooserAction.SAVE,
										texts, responses);
		
		if (dialog.run() != ResponseType.GTK_RESPONSE_OK) {
			dialog.hide();
			return;
		}

		Quadrant	q;
		graph.getQuadrant(q);
		ImageSurface		surface = ImageSurface.create(
										CairoFormat.RGB24, 
										q.length, q.length);
		Context				context = Context.create(surface);

		context.setSourceRgb(1.0, 1.0, 1.0);
		context.rectangle(0, 0, q.length, q.length);
		context.fill();
		context.setSourceRgb(0.2, 0.2, 0.2);
		context.setLineWidth(1.0);
		graph.expose(context);
		
		surface.writeToPng(dialog.getFilename());
		
		dialog.hide();
		context.destroy();
		surface.destroy();
		delete dialog;
	}

	void saveButtonClick(Button button) {
		char[][] texts;
		texts ~= "OK";
		texts ~= "Cancel";
		ResponseType[] responses;
		responses ~= ResponseType.GTK_RESPONSE_OK;
		responses ~= ResponseType.GTK_RESPONSE_CANCEL;
		FileChooserDialog	dialog = new FileChooserDialog("Save", 
										this, FileChooserAction.SAVE,
										texts, responses);
		
		if (dialog.run() != ResponseType.GTK_RESPONSE_OK) {
			dialog.hide();
			return;
		}
		
		saveGraph(graph, dialog.getFilename());
		
		dialog.destroy();
		delete dialog;
	}

	void openButtonClick(Button button) {
		char[][] texts;
		texts ~= "OK";
		texts ~= "Cancel";
		ResponseType[] responses;
		responses ~= ResponseType.GTK_RESPONSE_OK;
		responses ~= ResponseType.GTK_RESPONSE_CANCEL;
		FileChooserDialog	dialog = new FileChooserDialog("Open", 
										this, FileChooserAction.OPEN,
										texts, responses);
		
		if (dialog.run() != ResponseType.GTK_RESPONSE_OK) {
			dialog.hide();
			return;
		}
		
		delete graph;
		graph = loadGraph(dialog.getFilename());
		
		dialog.destroy();
		delete dialog;
		
		canvas.queueDraw();
	}
}
