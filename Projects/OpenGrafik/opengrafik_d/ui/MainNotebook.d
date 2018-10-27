/*
 *	This file is part of OpenGrafik.
 *
 *	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>
 *
 *	OpenGrafik is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	OpenGrafik is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OpenGrafik.  If not, see <http://www.gnu.org/licenses/>.
 */

module ui.MainNotebook;

private import
	gtk.Notebook,
	gtk.HBox,
	gtk.Label,
	gtk.Button,
	ui.DocumentViewport;
	
class MainNotebook: Notebook {
public:
	this () {
		super();
		setScrollable(true);
		popupEnable;
	}

	void newDocument (string label) {
		appendPage(new DocumentViewport, label);
	}
	
	// FIXME optimize
	void configureSize (int width, int height) {
		int pagesNum = getNPages;
		for (int i = 0; i < pagesNum; i++)
			(cast(DocumentViewport) getNthPage(i)).configureSize(width, height);
	}
}

