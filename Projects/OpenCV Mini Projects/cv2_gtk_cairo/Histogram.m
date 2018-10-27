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

#import "Histogram.h"
#import "Extras.h"
#import <stdio.h>
#import <highgui.h>
#import <cairo/cairo.h>

unsigned int getArrayMax (unsigned int *);
void renderCurve(cairo_t *, unsigned int *, int, int, int);
void renderBars(cairo_t *, unsigned int *, int, int, int);

@implementation Histogram

/**
 * Load the graphs from an image. If the image is grayscale,
 * using the G and cG (cumulative) channels to represent it.
 * Else, normally count the number of pixels with intensity
 * x and put the number in G[x]. Then, calculate the 
 * cumulative histogram into cX[].
 */
- loadFromImage: (const IplImage *) image {
	int				w = image->width,
					h = image->height;
	unsigned char	*d = (unsigned char *) image->imageData;

	c = image->nChannels;

	if (c == 1) {					/* grayscale image */
		for (int i = 0; i < w * h * c; i++)
			G[d[i]]++;

		cG[0] = G[0];
		for (int i = 1; i < 256; i++)
			cG[i] = cG[i - 1] + G[i];
	}
	else if (c == 3 || c == 4) {	/* color image */
		for (int i = 0; i < w * h * c; i += c) {
			B[d[i]]++;
			G[d[i + 1]]++;
			R[d[i + 2]]++;
		}

		cB[0] = B[0];
		cG[0] = G[0];
		cR[0] = R[0];
		for (int i = 1; i < 256; i++) {
			cB[i] = cB[i - 1] + B[i];
			cG[i] = cG[i - 1] + G[i];
			cR[i] = cR[i - 1] + R[i];
		}
	}

	return self;
}

/**
 * This renders the curves to a cairo context. Takes into 
 * consideration drawing area width and height to scale the 
 * histogram to it.
 */
- renderToCairoContext: (cairo_t *) cr 
			 withWidth: (int) width
			 andHeight: (int) height {
	unsigned int max = 0;

	cairo_set_line_width(cr, 1.0);
	cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
	cairo_rectangle(cr, 0, 0, width, height);
	cairo_fill(cr);

	if (c == 1) {					/* grayscale */
		max = getArrayMax(G);

		cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 0.7);
		renderBars(cr, G, max, width, height);			/* normal gray */

		cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 0.5);
		renderCurve(cr, cG, cG[255], width, height);	/* cumulative gray */
	}
	else if (c == 3 || c == 4) {	/* color */
		max = MAX(getArrayMax(R), MAX(getArrayMax(G), getArrayMax(B)));
		
		cairo_set_source_rgba(cr, 1.0, 0.0, 0.0, 0.5);
		renderCurve(cr, R, max, width, height);			/* normal red */
		
		cairo_set_source_rgba(cr, 0.0, 1.0, 0.0, 0.5);
		renderCurve(cr, G, max, width, height);			/* normal green */

		cairo_set_source_rgba(cr, 0.0, 0.0, 1.0, 0.5);
		renderCurve(cr, B, max, width, height);			/* normal blue */

		max = MAX(cR[255], MAX(cG[255], cB[255]));
		
		cairo_set_source_rgb(cr, 1.0, 0.0, 0.0);
		renderCurve(cr, cR, max, width, height);		/* cumulative red */

		cairo_set_source_rgb(cr, 0.0, 1.0, 0.0);
		renderCurve(cr, cG, max, width, height);		/* cumulative green */

		cairo_set_source_rgb(cr, 0.0, 0.0, 1.0);
		renderCurve(cr, cB, max, width, height);		/* cumulative blue */
	}

	return self;
}

/**
 * Return the first intensity with non-zero frequency in the grayscale 
 * histogram. Used for contrast stretching.
 */
- (unsigned char) stretchGetC {
	/* get first intensity with non-zero frequency */
	for (int i = 0; i < 256; i++)
		if (G[i] != 0)
			return i;
	return 0;
}

/**
 * Return the last intensity with non-zero frequency in the grayscale
 * histogram. Used for contrast stretching.
 */
- (unsigned char) stretchGetD {
	/* get last intensity with non-zero frequency */
	for (int i = 255; i >= 0; i--)
		if (G[i] != 0)
			return i;
	return 255;
}

/**
 * Gets the minimum non-zero cumulative frequency value.
 */
- (unsigned int) getMinCF {
	unsigned int min = cG[0];
	for (int i = 0; i < 256; i++)
		if (cG[i] < min && cG[i] != 0)
			min = cG[i];
	return min;
}

/**
 * Gets the cumulative frequency value of instensity.
 */
- (unsigned int) getCF: (unsigned char) intensity {
	return cG[intensity];
}

@end

/**
 * Gets the largest value in an array.
 */
unsigned int getArrayMax (unsigned int *array) {
	unsigned int result = 0;
	for (int i = 0; i < 256; i++) {
		if (array[i] > result)
			result = array[i];
	}
	return result;
}

/**
 * Renders a connected curve to a cairo context.
 */
void renderCurve(cairo_t *cr, unsigned int *array, int max, int width, int height) {
	for (int i = 0; i < 256; i++)
		cairo_line_to(cr, i * width / 255, height - (array[i] * height / max));
	cairo_stroke(cr);
}

/**
 * Renders bars (histogram) to a cairo context.
 */
void renderBars(cairo_t *cr, unsigned int *array, int max, int width, int height) {
	for (int i = 0; i < 256; i++) {
		cairo_move_to(cr, i * width / 255, height);
		cairo_line_to(cr, i * width / 255, height - (array[i] * height / max));
	}
	cairo_stroke(cr);
}

