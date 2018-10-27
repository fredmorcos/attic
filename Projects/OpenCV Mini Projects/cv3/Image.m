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

#import "Image.h"
#import "List.h"
#import <highgui.h>
#import <stdlib.h>
#import <stdio.h>
#import <cv.h>

#import "Window.h"
#import "config.h"

@implementation Image

/**
 * Image constructor, allocates everything.
 */
- init {
	[super init];
	return self;
}

/**
 * Image constructor, builds image object from 
 * IplImage.
 */
- initWithIplImage: (IplImage *) img {
	[self init];
	image = img;
	return self;
}

/**
 * Image constructor, allocates a Grayscale image with
 * width and height.
 */
- initGSWithWidth: (int) width andHeight: (int) height {
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
	return [super free];
}

/**
 * Sets all pixels in an image to 0;
 */
- reset {
	for (int i = 0; i < image->width; i++)
		for (int j = 0; j < image->height; j++)
			[self val: 0 toX: i andY: j];
	return self;
}

/**
 * Sets the value of a pixel (x, y) to val.
 */
- val: (int) val toX: (unsigned int) x andY: (unsigned int) y {
	int pos = y * image->widthStep + x;
	image->imageData[pos] = (unsigned char) val;
	return self;
}

/**
 * Returns the value of a pixel (x, y) or -1 if the requested
 * position is outside image boundaries.
 */
- (int) valFromX: (unsigned int) x andY: (unsigned int) y {
	if (x < 0 || x >= image->width ||
		y < 0 || y >= image->height)
		return -1;
	int pos = y * image->widthStep + x;
	return (unsigned char) image->imageData[pos];
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
 * Return the IplImage.
 */
- (IplImage *) image {
	return image;
}

/**
 * Returns whether the image is Grayscale or not.
 */
- (BOOL) isGS {
	return (image->nChannels == 1 && image->depth == 8);
}

/**
 * Returns whether the image is Binary or not. Assumes
 * that the image is binary if all values are _only_ 0
 * and 255.
 */
- (BOOL) isBin {
	if (image->nChannels != 1)
		return NO;
	for (int i = 0; i < image->width * image->height; i++)
		if ((unsigned char) image->imageData[i] != 255 &&
				(unsigned char) image->imageData[i] != 0)
			return NO;
	return YES;
}

/**
 * Prints the values of pixels out to the console.
 */
- print {
	for (int i = 0; i < image->height; i++) {
		for (int j = 0; j < image->width; j++)
			printf("%d\t", [self valFromX: j andY: i]);
		printf("\n");
	}

	return self;
}

/**
 * Returns a "Connected Compoenent Labeling" image consisting of pixel
 * labels of a two-pass algorithm.
 *
 * Loops over all pixels in the image. For each pixel, gets a linked list
 * of its neighbors. If it has no neighbors (i.e. pixel 0,0) then it uses
 * the label "counter". If the pixel has neighbors, will loop over them
 * and, if the pixel intensity values match (of the neighbor and the current
 * pixel), sets the label of the current pixel to the label of the matching
 * neighbor and keeps looping, adding the rest of the matching neighbors to a
 * label rule table. Non-matching neighbors are ignored. If all neighbors are
 * non-matching then it will use the label "counter".
 *
 * The label rule table is a table where if two connected pixels have the same
 * intensities but different labels, their labels are matched using the table.
 * This table is sorted automatically (i.e. for free - no sorting needed to be
 * done) due to the increasing nature of the label counter. The rules are
 * stored where min is in Match->src and max is in Match->dst.
 *
 * After that, the label image is looped over and each value is queried from
 * the rule table, returning its equivalent "original" value. This returned
 * value is used instead, connecting the connected parts that weren't detected
 * in the first pass.
 */
- ccl: (char *) filename {
	Image			*result = [[Image alloc] initAsCloneOfImage: self];
	List			*neighbors,
					*rules = [[List alloc] init],
					*features = [[List alloc] init];
	unsigned int	count = 0,
					tmp1,
					tmp2,
					tmpCol,
					numCol,
					area,
					largeIndex;
	CCLPixel		*pixel;
	Feature			*tmpFeat;
	Match			*rule;
	unsigned char	curVal;
	BOOL			useCount = YES,
					labeled = NO;
	FILE			*file;

#ifdef DEBUG
	[self print];
	printf("\n");
#endif

	/* first pass */
	for (int i = 0; i < image->height; i++) {
		for (int j = 0; j < image->width; j++) {
			neighbors = [self neighborsOfX: j andY: i];
			curVal = [self valFromX: j andY: i];
			useCount = YES;
			labeled = NO;
			if ([neighbors size] != 0) {
				for (int k = 0; k < [neighbors size]; k++) {
					pixel = (CCLPixel *) [neighbors getFromIndex: k];
					if (pixel->val == curVal) {
						if (labeled) {
							rule = malloc(sizeof(Match));
							tmp1 = [result valFromX: pixel->x andY: pixel->y];
							tmp2 = [result valFromX: j andY: i];
							rule->src = MIN(tmp1, tmp2);
							rule->dst = MAX(tmp1, tmp2);
							if (rule->src == rule->dst ||
									[Image rulesList: rules containsRule: rule])
								free(rule);
							else
								[rules add: rule];
						}
						else {
							[result val: [result valFromX: pixel->x
													 andY: pixel->y]
									toX: j andY: i];
							useCount = NO;
							labeled = YES;
						}
					}
				}

				if (useCount == YES)
					[result val: ++count toX: j andY: i];
			}
			else {
				[result val: ++count toX: j andY: i];
			}

			[neighbors freeDeep];
		}
	}

#ifdef DEBUG
	[result print];
	printf("\n");
#endif

	/* second pass */
	for (int i = 0; i < image->height; i++)
		for (int j = 0; j < image->width; j++)
			[result val:
				[Image getValueOf: [result valFromX: j andY: i]
					fromRulesList: rules] toX: j andY: i];
#ifdef DEBUG
	[result print];
	printf("\n");
#endif

#ifdef DEBUG
	Match *m;
	for (int i = 0; i < [rules size]; i++) {
		m = (Match *) [rules getFromIndex: i];
		printf("%d = %d\n", m->src, m->dst);
	}
#endif

	/* make a list of "features" included in the image (including the 
	 * background along with information about the objects such as the 
	 * positions of top-left and bottom-right corners, width and 
	 * height.
	 */
	for (int c = 1; c <= count; c++) {
		tmpFeat = malloc(sizeof(Feature));
		tmpFeat->x1 = image->width;
		tmpFeat->x2 = 0;
		tmpFeat->y1 = image->height;
		tmpFeat->y2 = 0;
		for (int i = 0; i < image->height; i++) {
			for (int j = 0; j < image->width; j++) {
				if ([result valFromX: j andY: i] == c) {
					if (j < tmpFeat->x1) tmpFeat->x1 = j;
					if (j > tmpFeat->x2) tmpFeat->x2 = j;
					if (i < tmpFeat->y1) tmpFeat->y1 = i;
					if (i > tmpFeat->y2) tmpFeat->y2 = i;
				}
			}
		}
		if (tmpFeat->x1 == image->width &&
			tmpFeat->x2 == 0 &&
			tmpFeat->y1 == image->height &&
			tmpFeat->y2 == 0)
			free(tmpFeat);
		else {
			tmpFeat->width = tmpFeat->x2 - tmpFeat->x1;
			tmpFeat->height = tmpFeat->y2 - tmpFeat->y1;
			[features add: tmpFeat];
		}
	}

	/* now the "features" are recognized by their positions and sizes, 
	 * we should categorize their types by looping over, each feature is 
	 * limited by a square/rectangle around it. so, circles will have 2 
	 * or more labels detected inside their boundary frames and 
	 * squares/rectangles will only have 1 label detected. this will treat 
	 * the background as a circle.
	 */
	for (int f = 0; f < [features size]; f++) {
		tmpFeat = (Feature *) [features getFromIndex: f];
		tmpCol = [result valFromX: tmpFeat->x1 andY: tmpFeat->y1];
		numCol = 1;
		for (int i = tmpFeat->x1; i < tmpFeat->x2; i++) {
			for (int j = tmpFeat->y1; j < tmpFeat->y2; j++) {
				if ([result valFromX: i andY: j] != tmpCol)
					++numCol;
			}
		}

		if (numCol > 1)
			tmpFeat->type = CIRCLE;
		else
			if (tmpFeat->width == tmpFeat->height)
				tmpFeat->type = SQUARE;
			else
				tmpFeat->type = RECTANGLE;
	}

	/* here we remove the background, it is the index with the largest area.
	 */
	tmpFeat = (Feature *) [features getFromIndex: 0];
	area = tmpFeat->width * tmpFeat->height;
	largeIndex = 0;
	for (int f = 1; f < [features size]; f++) {
		tmpFeat = (Feature *) [features getFromIndex: f];
		if (tmpFeat->width * tmpFeat->height > area) {
			area = tmpFeat->width * tmpFeat->height;
			largeIndex = f;
		}
	}
	((Feature *) [features getFromIndex: largeIndex])->type = NONE;
	
	/* open the file, loop over the features list and write the information 
	 * about the objects by appending, ignoring the NONE type (background).
	 */
	file = fopen(filename, "a");
	for (int i = 0; i < [features size]; i++) {
		tmpFeat = (Feature *) [features getFromIndex: i];
		switch(tmpFeat->type) {
			case CIRCLE:
				fprintf(file,
						"Circle with center [%d, %d] and radius %d.\n",
						tmpFeat->x1 + (tmpFeat->width / 2),
						tmpFeat->y1 + (tmpFeat->height / 2),
						tmpFeat->width / 2);
				break;
			case RECTANGLE:
				fprintf(file,
						"Rectangle at point [%d, %d], width %d and",
						tmpFeat->x1, tmpFeat->y1, tmpFeat->width);
				fprintf(file,
						" height %d.\n", tmpFeat->height);
				break;
			case SQUARE:
				fprintf(file,
						"Square at point [%d, %d] and size %d.\n",
						tmpFeat->x1, tmpFeat->y1, tmpFeat->width);
				break;
			default:
				// printf("Background.\n");
				break;
		}
	}

	/* free stuff and close file */
	fclose(file);
	[features freeDeep];
	[rules freeDeep];
	return self;
}

/**
 * This method takes a pixel position and returns a linked list of
 * the pixel's neighbors. Basically, if a neighbor position is out of
 * image bounds, it isn't added to the resulting list, else, its
 * intensity value and position are stored in the list (using CCLPixel).
 */
- (List *) neighborsOfX: (unsigned int) x andY: (unsigned int) y {
	List		*list = [[List alloc] init];
	CCLPixel	*pixel;
	int			value = -1;

	value = [self valFromX: x - 1 andY: y];
	if (value != -1) {
		pixel = malloc(sizeof(CCLPixel));
		pixel->x = x - 1;
		pixel->y = y;
		pixel->val = value;
		[list add: pixel];
	}

	value = [self valFromX: x - 1 andY: y - 1];
	if (value != -1) {
		pixel = malloc(sizeof(CCLPixel));
		pixel->x = x - 1;
		pixel->y = y - 1;
		pixel->val = value;
		[list add: pixel];
	}

	value = [self valFromX: x andY: y - 1];
	if (value != -1) {
		pixel = malloc(sizeof(CCLPixel));
		pixel->x = x;
		pixel->y = y - 1;
		pixel->val = value;
		[list add: pixel];
	}

	value = [self valFromX: x + 1 andY: y - 1];
	if (value != -1) {
		pixel = malloc(sizeof(CCLPixel));
		pixel->x = x + 1;
		pixel->y = y - 1;
		pixel->val = value;
		[list add: pixel];
	}

	return list;
}

/**
 * This is class method, given a list of label matching rules and a
 * rule, returns true or false based on whether the rule is in the table
 * or not. This is used to avoid duplicates.
 */
+ (BOOL) rulesList: (List *) list containsRule: (Match *) rule {
	Match *tmp;

	for (int i = 0; i < [list size]; i++) {
		tmp = (Match *) [list getFromIndex: i];
		if (tmp->src == rule->src && tmp->dst == rule->dst)
			return YES;
	}

	return NO;
}

/**
 * This is a class method, takes a list of label matching rules and a value,
 * the value is looked up in the list of rules recursively (if x = y and y = z
 * then x = z). So x is set to y, then y is set to z. This assumes that the
 * min value of a rule is stored in Match->src and max in Match->dst.
 */
+ (unsigned int) getValueOf: (unsigned int) value fromRulesList: (List *) list {
	Match *tmp;

	for (int i = [list size] - 1; i >= 0; i--) {
		tmp = (Match *) [list getFromIndex: i];
		if (tmp->dst == value)
			value = tmp->src;
	}

	return value;
}

/**
 * Returns an edges image created using cvCanny.
 */
- (Image *) applyOpenCVCannyWithT1: (double) t1 andT2: (double) t2 {
	Image		*result = [[Image alloc] initAsCloneOfImage: self];
	cvCanny (image, [result image], t1, t2, 3);
	return result;
}

/**
 * Returns an edges image using the Canny edge detection
 * algorithm. Applies Gaussian smoothing, Roberts edge 
 * detector, non-maximal suppression and hysteresis 
 * thresholding.
 */
- (Image *) applyCannyWithT1: (int) t1 andT2: (int) t2 {
	Image			*gaussianRes,
					*nonmaxRes;
	RobertsImage	*robertsRes;
	
	gaussianRes = [self applyGaussian];

#ifdef DEBUG
	[[[Window alloc] initWithName: "Gaussian"] showImage: gaussianRes];
#endif

	robertsRes = [gaussianRes applyRoberts];
	nonmaxRes = [robertsRes->image 
		applyNonMaximalSuppressionWithT1: t1 andT2: t2
						  andAnglesImage: robertsRes->angles];
#ifdef RECURSIVE
	for (int i = 0; i < image->width; i++)
		for (int j = 0; j < image->height; j++)
			if ([nonmaxRes valFromX: i andY: j] != 255)
				[nonmaxRes val: 0 toX: i andY: j];
#endif

	[gaussianRes free];
	[robertsRes->image free];
	[robertsRes->angles free];
	free(robertsRes);
	return nonmaxRes;
}

/**
 * Returns an image as the result of Gaussian smoothing. Uses a
 * 5x5 gaussian kernel and convolves the image with it. Note that 
 * it ignores border pixels are they are not useful for our 
 * application (Canny edge detection).
 */
- (Image *) applyGaussian {
	Image *result = [[Image alloc] initAsCloneOfImage: self];
	int 	gaussian[5][5] = {	{1,	4,	7,	4,	1},
								{4, 16, 26, 16, 4},
								{7, 26, 40, 26, 7},
								{4, 16, 26, 16, 4},
								{1,	4,	7,	4,	1}},
			gaussianSum = 272;
	double 	gaussianVal = 0.0;

	for (int i = 0; i < image->width; i++) {
		for (int j = 0; j < image->height; j++) {
			gaussianVal = 0.0;
			for (int ki = -2; ki < 3; ki++) {
				for (int kj = -2; kj < 3; kj++) {
					if (!(i + ki < 0 || j + kj < 0))
						gaussianVal += gaussian[ki + 2][kj + 2] * 
							[self valFromX: i + ki andY: j + kj];
				}
			}
			[result val: (int) (gaussianVal / gaussianSum)
					toX: i andY: j];
		}
	}

	return result;
}

/**
 * Applies Roberts edge detection and returns two images, a gradient 
 * image and an angles image. The gradient image contains the values 
 * of edges (magnitude) and the angles image contains the directions
 * of edges/pixels. The angles are labeled from 1 to 4 depending on 
 * the axis they comprise (x, y, 45/225, 135/315) with +/- 23 degrees.
 */
- (RobertsImage *) applyRoberts {
	RobertsImage	*result;
	id				img = [[Image alloc] initAsCloneOfImage: self],
					angles = [[Image alloc] initAsCloneOfImage: self];
	int				robertsX,
					robertsY,
					roberts,
					sobelX[3][3] = {	{-1,	0,	1},
										{-2,	0,	2},
										{-1,	0,	1}},
					sobelY[3][3] = {	{1,		2,	1},
										{0,		0,	0},
										{-1,	-2,	-1}};
	double			angle;

	for (int i = 0; i < image->width; i++) {
		for (int j = 0; j < image->height; j++) {
#ifdef SOBEL
			if ((!(i + 1 >= image->width || j + 1 >= image->height 
				|| i - 1 < 0 || j - 1 < 0))) {
//				robertsX = [self valFromX: i + 1 andY: j - 1] + 
//					  (2 * [self valFromX: i + 1 andY: j]) +
//						   [self valFromX: i + 1 andY: j + 1] -
//						   [self valFromX: i - 1 andY: j - 1] -
//					  (2 * [self valFromX: i - 1 andY: j]) -
//						   [self valFromX: i - 1 andY: j + 1];
//				robertsY = [self valFromX: i - 1 andY: j - 1] + 
//					  (2 * [self valFromX: i andY: j - 1]) +
//						   [self valFromX: i + 1 andY: j - 1] -
//						   [self valFromX: i - 1 andY: j + 1] -
//					  (2 * [self valFromX: i andY: j + 1]) -
//						   [self valFromX: i + 1 andY: j + 1];

				robertsX = 0;
				robertsY = 0;
				for (int ki = -1; ki <= 1; ki++) {
					for (int kj = -1; kj <= 1; kj++) {
						robertsX += [self valFromX: i + ki andY: j + kj] * 
							sobelX[1 + ki][1 + kj];
						robertsY += [self valFromX: i + ki andY: j + kj] * 
							sobelY[1 + ki][1 + kj];
					}
				}
#else
			if (!(i + 1 >= image->width || j + 1 >= image->height)) {
				robertsX = [self valFromX: i andY: j] - 
						   [self valFromX: i + 1 andY: j + 1];
				robertsY = [self valFromX: i + 1 andY: j] -
						   [self valFromX: i andY: j + 1];
#endif
				roberts = abs(robertsX) + abs(robertsY);
				if (roberts > 255) roberts = 255;
				[img val: roberts toX: i andY: j];
				angle = atan2(robertsY, robertsX) + 180;
// #ifdef SOBEL
//				angle = angle * 180 / 3.14;
// #else
//				angle -= ((3 * 3.14) / 4);
				angle = angle * 180 / 3.14;
// #endif
				while (angle > 360.0) angle -= 360.0;
				while (angle < 0.0) angle += 360.0;

				if (angle >= 337.5 || angle <= 22.5 ||
				   (angle >= 157.5 && angle <= 202.5))
					angle = 1;
				else if ((angle >= 22.5 && angle <= 67.5) ||
						(angle >= 202.5 && angle <= 247.5))
					angle = 2;
				else if ((angle >= 67.5 && angle <= 112.5) ||
						(angle >= 247.5 && angle <= 292.5))
					angle = 3;
				else if ((angle >= 112.5 && angle <= 157.5) ||
						(angle >= 292.5 && angle <= 337.5))
					angle = 4;
			}

			[angles val: angle toX: i andY: j];
		}
	}

	result = malloc(sizeof(RobertsImage));
	result->image = img;
	result->angles = angles;

#ifdef DEBUG
	[[[Window alloc] 
		initWithName: 
#ifdef SOBEL
		"Sobel"
#else
		"Roberts"
#endif
		] showImage: result->image];
#endif

	return result;
}

/**
 * Applies non-maximal suppression by filtering out and keeping in pixels
 * with values less than the lower threshold t2 and values larger than the 
 * higher threshold t1. Also keeps maximal-valued pixels along their 
 * directions in edges (depending on the angle and the values perpendicular 
 * to the angle direction). Then, applied hysteresis thresholding by 
 * following edges and removing pixels that do not take part in edges.
 */
- (Image *) applyNonMaximalSuppressionWithT1: (int) t1 andT2: (int) t2 
							  andAnglesImage: (Image *) angles {
	Image	*result1 = [[Image alloc] initAsCloneOfImage: self],
			*result2, *marked;
	int		angle, val, val1, val2, valh, valv, ivalh, ivalv, nextVal;

#ifdef DEBUG
#ifdef SOBEL
	for (int i = 0; i < image->width; i++)
		for (int j = 0; j < image->height; j++)
			[self val: MIN([self valFromX: i andY: j] / FACTOR, 255) 
				  toX: i andY: j];
	[[[Window alloc] initWithName: "Disintensified"] showImage: self];
#else
	for (int i = 0; i < image->width; i++)
		for (int j = 0; j < image->height; j++)
			[self val: MIN([self valFromX: i andY: j] * FACTOR, 255) 
				  toX: i andY: j];
	[[[Window alloc] initWithName: "Intensified"] showImage: self];
#endif
#endif

	for (int i = 0; i < image->width; i++) {
		for (int j = 0; j < image->height; j++) {
			if (i == 0 || j == 0
				|| i == image->width - 1 || j == image->height - 1
				|| [self valFromX: i andY: j] < t2)
				[result1 val: 0 toX: i andY: j];
			else {
				val = [self valFromX: i andY: j];
				angle = [angles valFromX: i andY: j];

				if (angle == 3) {
					val1 = [self valFromX: i andY: j - 1];
					val2 = [self valFromX: i andY: j + 1];
				}
				else if (angle == 2) {
					val1 = [self valFromX: i - 1 andY: j - 1];
					val2 = [self valFromX: i + 1 andY: j + 1];
				}
				else if (angle == 1) {
					val1 = [self valFromX: i - 1 andY: j];
					val2 = [self valFromX: i + 1 andY: j];
				}
				else if (angle == 4) {
					val1 = [self valFromX: i - 1 andY: j + 1];
					val2 = [self valFromX: i + 1 andY: j - 1];
				}

				if (val < val1 || val < val2)
					[result1 val: 0 toX: i andY: j];
//				else
//					[result1 val: 255 toX: i andY: j];
			}
		}
	}

	result2 = [[Image alloc] initAsCloneOfImage: result1];
#ifdef RECURSIVE
	marked = [[[Image alloc] initAsCloneOfImage: result2] reset];
#endif

#ifdef DEBUG
	Image *result3 = [[Image alloc] initAsCloneOfImage: result2];
	for (int i = 0; i < image->width; i++)
		for (int j = 0; j < image->height; j++)
			if ([result3 valFromX: i andY: j] > t2 &&
				[result3 valFromX: i andY: j] < t1)
				[result3 val: 255 toX: i andY: j];
			else
				[result3 val: 0 toX: i andY: j];
	[[[Window alloc] initWithName: "T2 < i < T1"] showImage: result3];
	result3 = [[Image alloc] initAsCloneOfImage: result2];
	for (int i = 0; i < image->width; i++)
		for (int j = 0; j < image->height; j++)
			if ([result3 valFromX: i andY: j] > t1)
				[result3 val: 255 toX: i andY: j];
			else
				[result3 val: 0 toX: i andY: j];
	[[[Window alloc] initWithName: "> T1"] showImage: result3];
	[[[Window alloc] initWithName: "Non-maximal Suppression"] showImage: result1];
#endif

	for (int i = 1; i < image->width - 1; i++) {
		for (int j = 1; j < image->height - 1; j++) {
			val = [result2 valFromX: i andY: j];
			if (val <= t2)
				[result2 val: 0 toX: i andY: j];
			else if (val >= t1) {
				[result2 val: 255 toX: i andY: j];
#ifdef RECURSIVE
				[result2 applyHysteresisWithT1: t1 andT2: t2
									  onPixelX: i andY: j
								andMarkerImage: marked];
			}
#else
			}
			else
				[result2 applyHysteresisWithT1: t1 andT2: t2
									  onPixelX: i andY: j
								andMarkerImage: marked];
#endif
		}
	}

	return result2;
}

/**
 * This is a recursive function that will follow edges and set to 1
 * the pixels that belong to the edge.
 */
- applyHysteresisWithT1: (int) t1 andT2: (int) t2 
			   onPixelX: (int) x andY: (int) y 
		 andMarkerImage: (Image *) marked {
	int val = 0;

#ifdef RECURSIVE
	val = [self valFromX: x andY: y];
	if ([marked valFromX: x andY: y] > 0 || val < t2) {
		[marked val: 1 toX: x andY: y];
		return self;
	}

	[self val: 255 toX: x andY: y];
	[marked val: 1 toX: x andY: y];

	for (int i = -1; i < 2; i++) {
		for (int j = -1; j < 2; j++) {
			[self applyHysteresisWithT1: t1 andT2: t2
							   onPixelX: x + i andY: y + j
						 andMarkerImage: marked];
		}
	}
	return self;
#else
	BOOL med = NO;

	for (int i = -1; i < 2; i++)
		for (int j = -1; j < 2; j++)
			if (!(i == 0 && j == 0)) {
				val = [self valFromX: x + i andY: y + j];
				if (val > t1) {
					[self val: 255 toX: x andY: y];
					return self;
				}
				else if (val < t1 && val > t2)
					med = YES;
			}

	if (med == YES) {
		for (int i = -2; i < 3; i++)
			for (int j = -2; j < 3; j++)
				if (i == -2 || i == 2 || j == -2 || j == 2)
					if ([self valFromX: x + i andY: y + j] > t2) {
						[self val: 255 toX: x andY: y];
						return self;
					}
	}

	[self val: 0 toX: x andY: y];
	return self;
#endif
}

@end

