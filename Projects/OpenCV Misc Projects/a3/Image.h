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

