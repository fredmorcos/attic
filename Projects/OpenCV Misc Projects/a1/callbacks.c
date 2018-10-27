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

#include "callbacks.h"
#include "ui-builder.h"
#include "cv-stuff.h"
#include <gtk/gtk.h>
#include <string.h>
#include <math.h>

void replicationRadio_toggle(GtkToggleButton *button, gpointer data) {
	GtkWidget *upsampleButton;

	upsampleButton = ui_get_widget("upsampleButton");
	gtk_widget_set_sensitive(upsampleButton, 
			gtk_toggle_button_get_active(button));
}

void scaleButton_click(GtkButton *button, gpointer data) {
	int		height,
			width;

	height = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON(
				ui_get_widget("scaleHeightSpin")));
	width = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON(
				ui_get_widget("scaleWidthSpin")));

	cv_stuff_scale(width, height);
}

void upsampleButtonCV_click(GtkButton *button, gpointer data) {
	gboolean	replication;
	double		factor;

	replication = gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON(
				ui_get_widget("replicationRadio")));
	factor = gtk_spin_button_get_value(
			GTK_SPIN_BUTTON(
				ui_get_widget("upsampleFactorSpin")));

	cv_stuff_upsample_opencv(replication, factor);
}

void upsampleButton_click(GtkButton *button, gpointer data) {
	gboolean			replication;
	double				factor;
	GtkMessageDialog	*dialog;

	replication = gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON(
				ui_get_widget("replicationRadio")));
	factor = gtk_spin_button_get_value(
			GTK_SPIN_BUTTON(
				ui_get_widget("upsampleFactorSpin")));

	if (isFloat(factor)) {
		factor = floor(factor);
		dialog = gtk_message_dialog_new(
				GTK_WINDOW(ui_get_widget("mainWindow")),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_INFO,
				GTK_BUTTONS_OK,
				"Factor will be rounded to %.1f.",
				factor);
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(GTK_WIDGET(dialog));
	}

	cv_stuff_upsample(replication, factor);
}

void rotateButton_click(GtkButton *button, gpointer data) {
	int		x,
			y;
	double	angle;

	x = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON(
				ui_get_widget("rotateXSpin")));
	y = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON(
				ui_get_widget("rotateYSpin")));
	angle = gtk_spin_button_get_value(
			GTK_SPIN_BUTTON(
				ui_get_widget("rotateAngleSpin")));

	cv_stuff_rotate(x, y, angle);
}

void rotateButtonCV_click(GtkButton *button, gpointer data) {
	int		x,
			y;
	double	angle;

	x = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON(
				ui_get_widget("rotateXSpin")));
	y = gtk_spin_button_get_value_as_int(
			GTK_SPIN_BUTTON(
				ui_get_widget("rotateYSpin")));
	angle = gtk_spin_button_get_value(
			GTK_SPIN_BUTTON(
				ui_get_widget("rotateAngleSpin")));

	cv_stuff_rotate_opencv(x, y, angle);
}

void negateButton1_click(GtkButton *button, gpointer data) {
	cv_stuff_negate1();
}

void negateButton3_click(GtkButton *button, gpointer data) {
	cv_stuff_negate3();
}

void negateButton2_click(GtkButton *button, gpointer data) {
	cv_stuff_negate2();
}

void menuAbout_activate(GtkMenuItem *item, gpointer data) {
	GtkWidget	*aboutDialog;

	aboutDialog = ui_get_widget("aboutDialog");

	if (gtk_dialog_run(GTK_DIALOG(aboutDialog)) == 
			GTK_RESPONSE_CANCEL)
		gtk_widget_hide(aboutDialog);
}

void menuOpen_activate(GtkMenuItem *item, gpointer data) {
	GtkWidget		*openDialog;
	GtkFileFilter	*filter;
	char			*filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter, "*.png");
	gtk_file_filter_add_pattern(filter, "*.bmp");

	openDialog = gtk_file_chooser_dialog_new(
			"Open Image",
			GTK_WINDOW(ui_get_widget("mainWindow")),
			GTK_FILE_CHOOSER_ACTION_OPEN,
			GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
			NULL);
	gtk_file_chooser_set_filter(
			GTK_FILE_CHOOSER(openDialog), filter);

	if (gtk_dialog_run(GTK_DIALOG(openDialog)) ==
			GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(openDialog));
		cv_stuff_window(filename);
	}

	gtk_widget_hide(openDialog);
}

void menuSave_activate(GtkMenuItem *item, gpointer data) {
	GtkWidget		*saveDialog;
	GtkFileFilter	*filter;
	char			*filename,
					*ext;
	GString			*tmp;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter, "*.jpg");
	gtk_file_filter_add_pattern(filter, "*.tif");

	saveDialog = gtk_file_chooser_dialog_new(
			"Save Image",
			GTK_WINDOW(ui_get_widget("mainWindow")),
			GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
			NULL);
	gtk_file_chooser_set_filter(
			GTK_FILE_CHOOSER(saveDialog), filter);
	gtk_file_chooser_set_do_overwrite_confirmation(
			GTK_FILE_CHOOSER(saveDialog), TRUE);

	if (gtk_dialog_run(GTK_DIALOG(saveDialog)) ==
			GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(saveDialog));
		
		tmp = g_string_new((const gchar *)filename);

		ext = &filename[strlen(filename) - 4];
		if (strcmp(ext, ".jpg") && strcmp(ext, ".tif"))
			g_string_append(tmp, ".jpg");
		cv_stuff_save_image(tmp->str);
		g_string_free(tmp, FALSE);
	}

	gtk_widget_hide(saveDialog);
}

