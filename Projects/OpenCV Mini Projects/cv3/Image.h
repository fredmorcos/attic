/*
	This file is part of cv3.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	cv3 is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cv3 is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cv3.  If not, see <http://www.gnu.org/licenses/>.
*/

#import <objc/Object.h>
#import <highgui.h>
#import "List.h"
#import "config.h"

typedef enum _FeatureType {
	NONE = 0,
	CIRCLE,
	SQUARE,
	RECTANGLE
} FeatureType;

typedef struct _CCLPixel {
	unsigned int x, y;
	unsigned char val;
} CCLPixel;

typedef struct _Match {
	unsigned int src, dst;
} Match;

typedef struct _Feature {
	FeatureType type;
	unsigned int x1, y1, x2, y2, width, height;
} Feature;

typedef struct _RobertsImage {
	id image, angles;
} RobertsImage;

@interface Image: Object {
@protected
	IplImage	*image;
}

- init;
- initWithIplImage: (IplImage *) img;
- initGSWithWidth: (int) width andHeight: (int) height;
- initAsCloneOfImage: (Image *) oldImage;
- initFromFile: (const char *) filename;
- free;

- reset;

- val: (int) val toX: (unsigned int) x andY: (unsigned int) y;
- (int) valFromX: (unsigned int) x andY: (unsigned int) y;

- (unsigned short) channels;
- (unsigned int) pixels;

- (IplImage *) image;

- (BOOL) isGS;
- (BOOL) isBin;

- print;

- ccl: (char *) filename;
- (List *) neighborsOfX: (unsigned int) x andY: (unsigned int) y;

- (Image *) applyOpenCVCannyWithT1: (double) t1 andT2: (double) t2;
- (Image *) applyCannyWithT1: (int) t1 andT2: (int) t2;
- (Image *) applyGaussian;
- (RobertsImage *) applyRoberts;
- (Image *) applyNonMaximalSuppressionWithT1: (int) t1 andT2: (int) t2 
							  andAnglesImage: (Image *) angles;
- applyHysteresisWithT1: (int) t1 andT2: (int) t2 
			   onPixelX: (int) x andY: (int) y 
		 andMarkerImage: (Image *) marked;

+ (BOOL) rulesList: (List *) list containsRule: (Match *) rule;
+ (unsigned int) getValueOf: (unsigned int) value fromRulesList: (List *) list;

@end

