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
	gtk.HBox, 
	gtk.Expander, 
	gtk.Button;

class UIFile: Expander {
protected:
	HBox	box;
	Button	exportButton,
			saveButton,
			openButton,
			quitButton;

public:
	this() {
		super("File");

		with (saveButton = new Button(StockID.SAVE, true)) {
		}

		with (openButton = new Button(StockID.OPEN, true)) {
		}

		with (exportButton = new Button(StockID.CONVERT, true)) {
		}

		with (quitButton = new Button(StockID.QUIT, true)) {
		}

		with (box = new HBox(false, 5)) {
			packStart(openButton, false, false, 0);
			packStart(saveButton, false, false, 0);
			packStart(exportButton, false, false, 0);
			packStart(quitButton, false, false, 0);
		}

		add(box);
	}

	~this() {
		delete saveButton;
		delete exportButton;
		delete openButton;
		delete quitButton;
		delete box;
	}

	Button getQuitButton() {
		return quitButton;
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

