/*
	This file is part of cv1-gtk-cairo.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	cv1-gtk-cairo is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cv1-gtk-cairo is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cv1-gtk-cairo.  If not, see <http://www.gnu.org/licenses/>.
*/

#import <stdio.h>
#import <stdlib.h>
#import <gtk/gtk.h>
#import <objc/objc.h>

#import "UIBuilder.h"
#import "Window.h"
#import "Image.h"

gboolean histArea_expose (GtkWidget *, GdkEventExpose *, gpointer);
void renderToFile(const char *, Histogram *);

int main (int argc, char *argv[]) {
	GtkWidget 	*histWin,
			  	*srcHistArea,
			  	*dstHistArea,
				*srcVHistArea,
				*dstVHistArea,
				*label,
				*box;
	UIBuilder	*ui;
	Window		*srcWin,
				*dstWin;
	Image		*srcImg,
				*dstImg,
				*vImg,
				*vImgEQ;

	if (argc < 2) {		/* incorrect number of parameters */
		printf("Usage: ./cv2 <image-filename>\n");
		exit(1);
	}

	/* source image stuff */
	printf("Loading image from file %s... ", argv[1]);
	srcImg = [[Image alloc] initFromFile: argv[1]];
	if ([srcImg isGS])
		printf("[Grayscale] ");
	else
		printf("[Color] ");
	[srcImg loadHistogram];
	renderToFile("histogram-original.png", [srcImg histogram]);
	srcWin = [[[Window alloc] initWithName: "Source Image"] showImage: srcImg];
	printf("DONE\n");

	if ([srcImg isGS]) {		/* grayscale image */
		if (argc < 4) {			/* arguments not given (or not complete) */
			printf("Stretching Image to Minimum 0 and Maximum 255... ");
			dstImg = [srcImg stretchWithMin: 0 andMax: 255];
		}
		else {
			printf("Stretching Image to Minimum %d and Maximum %d... ", atoi(argv[2]), atoi(argv[3]));
			dstImg = [srcImg stretchWithMin: atoi(argv[2]) andMax: atoi(argv[3])];
		}
		[dstImg loadHistogram];
		renderToFile("histogram-stretched.png", [dstImg histogram]);
		dstWin = [[[Window alloc] initWithName: "Streched Image"] showImage: dstImg];
		printf("DONE\n");
	}
	else {						/* color image */
		printf("Equalizing Image... ");
		vImg = [srcImg vImage];
		[vImg loadHistogram];
		renderToFile("histogram-v-original.png", [vImg histogram]);
		vImgEQ = [srcImg vEQ: vImg];
		[vImgEQ loadHistogram];
		renderToFile("histogram-v-equalized.png", [vImgEQ histogram]);
		dstImg = [srcImg equalize: vImg];
		[dstImg loadHistogram];
		renderToFile("histogram-equalized.png", [dstImg histogram]);
		dstWin = [[[Window alloc] initWithName: "Equalized Image"] showImage: dstImg];
		printf("DONE\n");
	}

	gtk_init(&argc, &argv);

	printf("Loading UI... ");
	ui = [[UIBuilder alloc] init];
	if ([ui addFromFile: "ui.xml"] == NO) {
		printf("ERROR\n");
		[ui free];
		exit(1);
	}

	box = [ui getWidget: "vbox2"];
	label = [ui getWidget: "label3"];
	if ([srcImg isGS]) {
		gtk_label_set_label(GTK_LABEL(label), "<b>Stretched Histogram</b>");
		gtk_widget_hide(box);
	}

	srcHistArea = [ui getWidget: "srcHistArea"];
	dstHistArea = [ui getWidget: "dstHistArea"];
	srcVHistArea = [ui getWidget: "srcVHistArea"];
	dstVHistArea = [ui getWidget: "dstVHistArea"];
	histWin = [ui getWidget: "histWin"];
	if (!histWin || !srcHistArea || !dstHistArea || !dstVHistArea || !srcVHistArea 
		|| !label || !box) {
		printf("ERROR\n");
		[ui free];
		exit(1);
	}
	printf("DONE\n");

	/*
	 * all the rendering callbacks are the same function,
	 * just given different rendering areas and different 
	 * histograms to render.
	 */
	g_signal_connect(
			G_OBJECT(histWin), 
			"delete-event",
			G_CALLBACK(gtk_main_quit),
			NULL);
	g_signal_connect(
			G_OBJECT(srcHistArea),
			"expose-event",
			G_CALLBACK(histArea_expose),
			srcImg);
	g_signal_connect(
			G_OBJECT(dstHistArea),
			"expose-event",
			G_CALLBACK(histArea_expose),
			dstImg);
	if (![srcImg isGS]) {
		g_signal_connect(
				G_OBJECT(srcVHistArea),
				"expose-event",
				G_CALLBACK(histArea_expose),
				vImg);
		g_signal_connect(
				G_OBJECT(dstVHistArea),
				"expose-event",
				G_CALLBACK(histArea_expose),
				vImgEQ);
	}

	gtk_widget_show(histWin);
	gtk_main();

	/* free and finish */
	gtk_widget_destroy(histWin);
	[ui free];
	if (![srcImg isGS]) {
		[vImg free];
		[vImgEQ free];
	}
	[dstWin free];
	[dstImg free];
	[srcWin free];
	[srcImg free];
	[Window destroyAll];
	return 0;
}

/**
 * Rendering callback.
 */
gboolean histArea_expose (GtkWidget *widget, GdkEventExpose *event, gpointer user_data) {
	cairo_t	*cr = gdk_cairo_create(gtk_widget_get_window(widget));
	Image	*img = (Image *) user_data;
	[[img histogram] renderToCairoContext: cr 
							   	withWidth: widget->allocation.width 
								andHeight: widget->allocation.height];
	cairo_destroy(cr);
	return FALSE;
}

/**
 * Render to file.
 */
void renderToFile(const char *filename, Histogram *hist) {
	cairo_surface_t	*sr = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1000, 1000);
	cairo_t 		*cr = cairo_create(sr);
	[hist renderToCairoContext: cr
					 withWidth: 1000
					 andHeight: 1000];
	cairo_surface_write_to_png(sr, filename);
	cairo_destroy(cr);
}

