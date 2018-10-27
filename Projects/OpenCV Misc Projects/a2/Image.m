#import "Image.h"
#import "Histogram.h"
#import "Extras.h"
#import <highgui.h>
#import <stdlib.h>

unsigned char stretchV (unsigned char, double, double, double, double);
unsigned char equalizeV (unsigned char, unsigned int, unsigned int, unsigned int);

@implementation Image

/**
 * Image constructor, allocates everything.
 */
- init {
	[super init];
	histogram = [[Histogram alloc] init];
	return self;
}

/**
 * Image constructor, allocates an RGB image with width and
 * height.
 */
- initRGBWithWidth: (int) width andHeight: (int) height {
	[self init];
	image = cvCreateImage(cvSize(width, height), 8, 3);
	return self;
}

/**
 * Image constructor, allocates a Grayscale image with 
 * width and height.
 */
- initGrayWithWidth: (int) width andHeight: (int) height {
	[self init];
	image = cvCreateImage(cvSize(width, height), 8, 1);
	return self;
}

/**
 * Image constructor, clones an image into this one.
 */
- initAsCloneOfImage: (Image *) oldImage {
	[self init];
	image = cvCloneImage([oldImage image]);
	return self;
}

/**
 * Image constructor, loads an image from file.
 */
- initFromFile: (const char *) filename {
	[self init];
	image = cvLoadImage(filename, -1);
	return self;
}

/**
 * Image destructor, frees the image and its histogram.
 */
- free {
	cvReleaseImage(&image);
	[histogram free];
	return [super free];
}

/**
 * Sets point in image to color in HSV. This is done by 
 * converting color to RGB using HSVtoRGB (in Extras.m) 
 * then setting it to point.
 */
- HSV: (HSVColor *) color toPoint: (Point *) point {
	RGBColor *tmp = HSVtoRGB(color);
	[self RGB: tmp toPoint: point];
	free(tmp);
	return self;
}

/**
 * Sets the RGB value of point to color. This is straight-forward
 * pixel value setting.
 */
- RGB: (RGBColor *) color toPoint: (Point *) point {
	int pos = point->Y * image->widthStep + point->X * image->nChannels;
	image->imageData[pos + CR] = (unsigned char) color->R;
	image->imageData[pos + CG] = (unsigned char) color->G;
	image->imageData[pos + CB] = (unsigned char) color->B;
	return self;
}

/**
 * Sets the value of point to color. This is straight-forward
 * pixel value setting. Grayscale.
 */
- GS: (unsigned char) color toPoint: (Point *) point {
	int pos = point->Y * image->widthStep + point->X * image->nChannels;
	image->imageData[pos + 0] = color;
	return self;
}

/**
 * Gets the HSV value in image at point. This is done 
 * by getting the RGB value and converting it to HSV
 * using the function in Extras.m.
 */
- (HSVColor *) HSV: (Point *) point {
	RGBColor *tmp = [self RGB: point];
	HSVColor *tmp2 = RGBtoHSV(tmp);
	free(tmp);
	return tmp2;
}

/**
 * Gets the RGB value of point, straight-forward value reading.
 */
- (RGBColor *) RGB: (Point *) point {
	int pos = point->Y * image->widthStep + point->X * image->nChannels;
	return RGB(
		(unsigned char) image->imageData[pos + CR],
		(unsigned char) image->imageData[pos + CG],
		(unsigned char) image->imageData[pos + CB]);
}

/**
 * Gets the value of point, straight-forward value reading. Grayscale.
 */
- (unsigned char) GS: (Point *) point {
	int pos = point->Y * image->widthStep + point->X * image->nChannels;
	return (unsigned char) image->imageData[pos + 0];
}

/**
 * Return the number of channels of the image.
 */
- (unsigned short) channels {
	return (unsigned short) image->nChannels;
}

/**
 * Return the number of pixels in the image.
 */
- (unsigned int) pixels {
	return image->width * image->height;
}

/**
 * Return the histogram.
 */
- (Histogram *) histogram {
	return histogram;
}

/**
 * Return the IplImage.
 */
- (IplImage *) image {
	return image;
}

/**
 * Pass the image to the histogram class to generate/prepare and plot it.
 */
- loadHistogram {
	if (image)
		[histogram loadFromImage: image];
	return self;
}

/**
 * Returns an image stretched with minimum a and maximum b. 
 * Applies contrast stretching to each pixel individually.
 * Explained in report.
 */
- (Image *) stretchWithMin: (int) a andMax: (int) b {
	Image		*img = [[Image alloc] initGrayWithWidth: image->width
											  andHeight: image->height];
	IplImage	*tmp = [img image];
	int			c = [histogram stretchGetC],
				d = [histogram stretchGetD];

	for (int i = 0; i < image->width * image->height; i++) {
		tmp->imageData[i] = stretchV(image->imageData[i], a, b, c, d);
	}
	return img;
}

/**
 * Returns an Image of the V component of the Image. This is done by 
 * converting every pixel to HSV and creating an image with the V 
 * values only. This is used to calculate the equalized V values and 
 * to draw the V histograms.
 */
- (Image *) vImage {
	Image		*img = [[Image alloc] initGrayWithWidth: image->width 
											  andHeight: image->height];
	for (int i = 0; i < image->width; i++)
		for (int j = 0; j < image->height; j++) {
			[img GS: (RGBtoHSV([self RGB: POINT(i, j)]))->V toPoint: POINT(i, j)];
		}
	return img;
}

/**
 * This returns an equalized RGB image. The equalized image is generated 
 * by equalizing pixels individually using a V image.
 */
- (Image *) equalize: (Image *) vImage {
	Image *img = [[Image alloc] initRGBWithWidth: image->width
									   andHeight: image->height];
	HSVColor *hsv;

	for (int i = 0; i < image->width; i++)
		for (int j = 0; j < image->height; j++) {
			hsv = [self HSV: POINT(i, j)];
			hsv->V = equalizeV(hsv->V, [[vImage histogram] getCF: hsv->V], 
					[[vImage histogram] getMinCF], [vImage pixels]);
			[img HSV: hsv toPoint: POINT(i, j)];
		}
	return img;
}

/**
 * Returns an equalized V image from a normal V image. Used to generate
 * the equalized HSV image and used to generate the equalized V histogram.
 */
- (Image *) vEQ: (Image *) vImage {
	Image *img = [[Image alloc] initGrayWithWidth: image->width
										andHeight: image->height];
	HSVColor *hsv;

	for (int i = 0; i < image->width; i++)
		for (int j = 0; j < image->height; j++) {
			hsv = [self HSV: POINT(i, j)];
			hsv->V = equalizeV(hsv->V, [[vImage histogram] getCF: hsv->V], 
					[[vImage histogram] getMinCF], [vImage pixels]);
			[img GS: hsv->V toPoint: POINT(i, j)];
		}
	return img;
}

/**
 * Returns whether the image is Grayscale or not (color).
 */
- (BOOL) isGS {
	if (image->nChannels == 1)
		return YES;
	else
		return NO;
}

@end

/**
 * Applies contrast stretching to an individual value/pixel.
 */
unsigned char stretchV (unsigned char i, double a, double b, double c, double d) {
	return (i - c) * ((b - a) / (d - c)) + a;
}

/**
 * Applies histogram equalization to an individual value/pixel.
 */
unsigned char equalizeV (unsigned char i, unsigned int f, unsigned int fmin,
							unsigned int numPixels) {
	return (f - fmin) * (255.0 / (numPixels - fmin));
}

