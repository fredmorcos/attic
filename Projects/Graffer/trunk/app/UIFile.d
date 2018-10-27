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

module app.UIFile;

private import
	gtk.VBox, 
	gtk.HBox,
	gtk.Expander, 
	gtk.Button;

class UIFile: Expander {
protected:
	VBox	box;
	HBox	fileBox;
	Button	exportButton,
			saveButton,
			openButton;

public:
	this() {
		super("File");

		with (openButton = new Button(StockID.OPEN, false)) {
		}

		with (saveButton = new Button(StockID.SAVE, false)) {
		}

		with (exportButton = new Button(StockID.CONVERT, false)) {
		}

		with (fileBox = new HBox(false, 5)) {
			packStart(openButton, true, true, 0);
			packStart(saveButton, true, true, 0);
		}

		with (box = new VBox(false, 5)) {
			packStart(fileBox, false, false, 0);
			packStart(exportButton, true, true, 0);
		}

		add(box);
	}

	~this() {
		delete saveButton;
		delete exportButton;
		delete openButton;
		delete box;
	}

	Button getSaveButton() {
		return saveButton;
	}

	Button getOpenButton() {
		return openButton;
	}

	Button getExportButton() {
		return exportButton;
	}
}

