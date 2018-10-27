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
#import <cairo/cairo.h>

@interface Histogram: Object {
@protected
	unsigned short	c;
	unsigned int	B[256],		/* Blue frequencies */
					G[256],		/* Green or Gray frequencies */
					R[256],		/* Red frequencies */
					cB[256],	/* cumulative Blue */
					cG[256],	/* cumulative Green or Gray */
					cR[256];	/* cumulative Red */
}

- loadFromImage: (const IplImage *) image;
- renderToCairoContext: (cairo_t *) cr 
			 withWidth: (int) width 
			 andHeight: (int) height;

- (unsigned char) stretchGetC;
- (unsigned char) stretchGetD;
- (unsigned int) getMinCF;
- (unsigned int) getCF: (unsigned char) intensity;

@end
