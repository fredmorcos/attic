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
