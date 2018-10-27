/*
	This file is part of cv-a1.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	cv-a1 is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cv-a1 is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cv-a1.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "ui-builder.h"
#include "callbacks.h"
#include "cv-stuff.h"
#include <gtk/gtk.h>

int main (int argc, char *argv[]) {
	GtkWidget	*mainWindow,
				*negateButton1,
				*negateButton2,
				*negateButton3,
				*rotateButton,
				*rotateButtonCV,
				*upsampleButton,
				*upsampleButtonCV,
				*scaleButton,
				*replicationRadio;
	GtkAction	*menuOpen,
				*menuSave,
				*menuQuit,
				*menuAbout;

	gtk_init(&argc, &argv);

	ui_init();
	mainWindow = ui_get_widget("mainWindow");
	negateButton1 = ui_get_widget("negateButton1");
	negateButton2 = ui_get_widget("negateButton2");
	negateButton3 = ui_get_widget("negateButton3");
	rotateButton = ui_get_widget("rotateButton");
	rotateButtonCV = ui_get_widget("rotateButtonCV");
	upsampleButton = ui_get_widget("upsampleButton");
	upsampleButtonCV = ui_get_widget("upsampleButtonCV");
	scaleButton = ui_get_widget("scaleButton");
	replicationRadio = ui_get_widget("replicationRadio");

	menuOpen = ui_get_action("menuOpen");
	menuSave = ui_get_action("menuSave");
	menuQuit = ui_get_action("menuQuit");
	menuAbout = ui_get_action("menuAbout");

	/* connect signals */
	g_signal_connect(G_OBJECT(mainWindow), "delete-event",
			G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(menuQuit), "activate",
			G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(menuOpen), "activate",
			G_CALLBACK(menuOpen_activate), NULL);
	g_signal_connect(G_OBJECT(menuSave), "activate",
			G_CALLBACK(menuSave_activate), NULL);
	g_signal_connect(G_OBJECT(menuAbout), "activate",
			G_CALLBACK(menuAbout_activate), NULL);
	g_signal_connect(G_OBJECT(negateButton1), "clicked",
			G_CALLBACK(negateButton1_click), NULL);
	g_signal_connect(G_OBJECT(negateButton2), "clicked",
			G_CALLBACK(negateButton2_click), NULL);
	g_signal_connect(G_OBJECT(negateButton3), "clicked",
			G_CALLBACK(negateButton3_click), NULL);
	g_signal_connect(G_OBJECT(rotateButton), "clicked",
			G_CALLBACK(rotateButton_click), NULL);
	g_signal_connect(G_OBJECT(rotateButtonCV), "clicked",
			G_CALLBACK(rotateButtonCV_click), NULL);
	g_signal_connect(G_OBJECT(upsampleButton), "clicked",
			G_CALLBACK(upsampleButton_click), NULL);
	g_signal_connect(G_OBJECT(upsampleButtonCV), "clicked",
			G_CALLBACK(upsampleButtonCV_click), NULL);
	g_signal_connect(G_OBJECT(scaleButton), "clicked",
			G_CALLBACK(scaleButton_click), NULL);
	g_signal_connect(G_OBJECT(replicationRadio), "clicked",
			G_CALLBACK(replicationRadio_toggle), NULL);

	gtk_widget_show_all(mainWindow);
	gtk_main();

	ui_destroy();
	cv_stuff_destroy();

	return 0;
}

