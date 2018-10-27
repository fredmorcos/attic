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

module app.UIGraph;

private import
	graph.adt.Graph,
	gtk.VBox, 
	gtk.ComboBox, 
	gtk.Label, 
	gtk.SpinButton, 
	gtk.Expander,
	gtk.Button, 
	gtk.HBox;

class UIGraph: Expander {
protected:
	VBox		box;
	HBox		verticesBox,
				edgesBox;
	SpinButton	verticesSpin;
	ComboBox	edgesCombo;
	Button		edgesButton,
				verticesButton;

public:
	this() {
		super("Graph");
		
		with (verticesSpin = new SpinButton(0, 100000, 10)) {
			setValue(100);
		}

		with (verticesButton = new Button(StockID.APPLY, true)) {
		}

		with (verticesBox = new HBox(false, 5)) {
			packStart(new Label("Vertices"), false, false, 0);
			packStart(verticesSpin, true, true, 0);
			packStart(verticesButton, false, false, 0);		
		}

		with (edgesCombo = new ComboBox()) {
			appendText("None");
			appendText("Circular");
			appendText("Centered");
			appendText("Interconnected");
			appendText("Binary Tree");
			setActive(0);
		}

		with (edgesButton = new Button(StockID.APPLY, true)) {
		}

		with (edgesBox = new HBox(false, 5)) {
			packStart(new Label("Edges"), false, false, 0);
			packStart(edgesCombo, true, true, 0);
			packStart(edgesButton, false, false, 0);
		}

		with (box = new VBox(false, 5)) {
			packStart(verticesBox, false, false, 0);
			packStart(edgesBox, false, false, 0);
		}

		add(box);
	}

	~this() {
		delete verticesSpin;
		delete edgesCombo;
		delete edgesButton;
		delete verticesButton;
		delete verticesBox;
		delete edgesBox;
		delete box;
	}

	Button getVerticesButton() {
		return verticesButton;
	}

	Button getEdgesButton() {
		return edgesButton;
	}

	int getNumberOfVertices() {
		return verticesSpin.getValueAsInt();
	}

	GraphShape getGraphShape() {
		switch(edgesCombo.getActiveText()) {
			case "Circular":			return GraphShape.CIRCULAR;
			case "Centered":			return GraphShape.CENTERED;
			case "Interconnected":		return GraphShape.INTERCONNECTED;
			case "Binary Tree":			return GraphShape.BINARYTREE;
			default:					return GraphShape.NONE;
		}
	}
}

