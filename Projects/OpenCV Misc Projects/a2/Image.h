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

