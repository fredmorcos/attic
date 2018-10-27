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

#import <objc/Object.h>
#import <highgui.h>
#import "Histogram.h"
#import "Extras.h"

@interface Image: Object {
@protected
	IplImage	*image;
	Histogram	*histogram;
}

- init;
- initRGBWithWidth: (int) width andHeight: (int) height;
- initGrayWithWidth: (int) width andHeight: (int) height;
- initAsCloneOfImage: (Image *) oldImage;
- initFromFile: (const char *) filename;
- free;

- HSV: (HSVColor *) color toPoint: (Point *) point;
- RGB: (RGBColor *) color toPoint: (Point *) point;
- GS: (unsigned char) color toPoint: (Point *) point;

- (HSVColor *) HSV: (Point *) point;
- (RGBColor *) RGB: (Point *) point;
- (unsigned char) GS: (Point *) point;

- (unsigned short) channels;
- (unsigned int) pixels;

- (Histogram *) histogram;
- (IplImage *) image;

- loadHistogram;

- (Image *) stretchWithMin: (int) a andMax: (int) b;
- (Image *) vImage;

- (BOOL) isGS;

- (Image *) equalize: (Image *) vImage;
- (Image *) vEQ: (Image *) vImage;

@end

