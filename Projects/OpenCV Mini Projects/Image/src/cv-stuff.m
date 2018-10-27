/*
	This file is part of imageman.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	imageman is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	imageman is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with imageman.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "cv-stuff.h"
#include <cv.h>
#include <highgui.h>
#include <glib.h>
#include <math.h>

#define TYPE CV_32FC1

static IplImage *image;		/* currently set image */

/**
 * Creates a window and loads an image from filename
 * into it.
 */
void cv_stuff_window(char *filename) {
	cv_stuff_destroy();
	image = cvLoadImage(filename, -1);
	cvNamedWindow("Image", CV_WINDOW_AUTOSIZE);
	cvShowImage("Image", image);
}

/**
 * Destroys and releases the image memory if there 
 * is any.
 */
void cv_stuff_destroy() {
	if (image) cvReleaseImage(&image);
}

/**
 * Reloads the image data into the window.
 */
void cv_stuff_reload() {
	if (!image) return;

	cvShowImage("Image", image);
}

/**
 * Saves image data to a filename.
 */
void cv_stuff_save_image(char *filename) {
	if (image) cvSaveImage(filename, image);
}

/**
 * Checks if the window is still open, if not, 
 * then free the image.
 */
void cv_stuff_check() {
	if (!image) return;

	void *handle;

	handle = cvGetWindowHandle("Image");

	if (!handle)
		cv_stuff_destroy();
}

/**
 * Inverts an image (gets its negative) by 
 * going over all pixels in the image data
 * and doing a val = 255 - val.
 */
void cv_stuff_negate1() {
	cv_stuff_check();
	if (!image) return;

	int		w,		/* width */ 
			h, 		/* height */
			s, 		/* step */
			c,		/* channels */
			i,		/* i counter */
			j,		/* j counter */
			k,		/* k counter */
			p;		/* pos in array */
	uchar	*d;		/* image data */

	w = image->width;
	h = image->height;
	s = image->widthStep;
	c = image->nChannels;
	d = (uchar *)image->imageData;

	/**
	 * Invert every pixel in the image. Deals 
	 * with the image as a 2D matrix with 
	 * direct access.
	 */
	for (i = 0; i < h; i++)
		for (j = 0; j < w; j++)
			for (k = 0; k < c; k++) {
				p = i * s + j * c + k;
				d[p] = 255 - d[p];
			}

	cv_stuff_reload();
}



/**
 * Inverts an image (gets its negative) by 
 * going over all pixels in the image data
 * and doing a val = 255 - val.
 */
void cv_stuff_negate2() {
	cv_stuff_check();
	if (!image) return;

	int		w,		/* width */ 
			h, 		/* height */
			s, 		/* step */
			c,		/* channels */
			i,		/* i counter */
			j,		/* j counter */
			k,		/* k counter */
			p;		/* pos in array */
	uchar	*d;		/* image data */

	w = image->width;
	h = image->height;
	s = image->widthStep;
	c = image->nChannels;
	d = (uchar *)image->imageData;

	/**
	 * Another way to get the inverse of an image,
	 * though slower. Deals with the image as a 
	 * 2D matrix with indirect access.
	 */	
	CvScalar pixel;
	for (i = 0; i < w; i++)
		for (j = 0; j < h; j++) {
			pixel = cvGet2D(image, j, i);
			for (k = 0; k < c; k++)
				pixel.val[k] = 255 - pixel.val[k];
			cvSet2D(image, j, i, pixel);
		}

	cv_stuff_reload();
}

/**
 * Inverts an image (gets its negative) by 
 * going over all pixels in the image data
 * and doing a val = 255 - val.
 */
void cv_stuff_negate3() {
	cv_stuff_check();
	if (!image) return;

	int		w,		/* width */ 
			h, 		/* height */
			c,		/* channels */
			i;		/* counter */
	uchar	*d;		/* image data */

	w = image->width;
	h = image->height;
	c = image->nChannels;
	d = (uchar *)image->imageData;

	/**
	 * Invert every pixel in the image. Deals 
	 * with the image as a 1D matrix.
	 */
	for (i = 0; i < h * w * c; i++)
		d[i] = 255 - d[i];

	cv_stuff_reload();
}

/**
 * Receives the position of a pixel in float and interpolates 
 * its intensity value (for each channel) using the 
 * neighboring pixels.
 */
CvScalar interpolate (IplImage *image, float x, float y) {
	int xf = floorf(x),		/* left */
		xc = ceilf(x),		/* right */
		yf = floorf(y),		/* top */
		yc = ceilf(y);		/* bottom */

	double l = x - xf,		/* left */
		   r = xc - x,		/* right */
		   u = y - yf,		/* up */
		   d = yc - y;		/* down */

	int	k;					/* channels counter */

	double v_up,			/* vertical interpolation, up */
		   v_down;			/* vertical interpolation, down */

	CvScalar res;

	for (k = 0; k < image->nChannels; k++) {
		v_up = (l * cvGet2D(image, xc, yf).val[k]) + (r * cvGet2D(image, xf, yf).val[k]);
		v_down = (l * cvGet2D(image, xc, yc).val[k]) + (r * cvGet2D(image, xf, yc).val[k]);
		res.val[k] = (u * v_down) + (d * v_up);
	}

	return res;
}

/**
 * Rotates an image around point (x, y) by angle. Uses 
 * OpenCV built-in functions to do so. Unused.
 */
void cv_stuff_rotate_opencv(int x, int y, double angle) {
	cv_stuff_check();
	if (!image) return;

	IplImage		*tmp;
	CvPoint2D32f	rot_point;
	CvMat			*rot_matrix;

	rot_point = cvPoint2D32f(x, y);
	rot_matrix = cvCreateMat(2, 3, CV_32FC1);
	cv2DRotationMatrix(rot_point, angle, 1.0, rot_matrix);

	tmp = cvCloneImage(image);
	cvWarpAffine(tmp, image, rot_matrix, 
			CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS, cvScalarAll(0));

	cvReleaseImage(&tmp);
	cv_stuff_reload();
}

/**
 * Rotates an image around point (x, y) by angle
 * manually. First creates a translation matrix to 
 * move the data by (-x, -y). Then creates a rotation
 * matrix to rotate by angle then translates back by 
 * (x, y). Then, it multiplies the three matrices 
 * together and gets the inverse to get the intensity 
 * of the original pixel into the destination one.
 * Interpolation is used in the last process to smooth 
 * the resulting image.
 */
void cv_stuff_rotate(int y, int x, double angle) {
	cv_stuff_check();
	if (!image) return;

	int i, j;
	float a = angle * M_PI / 180;
	float src_p[3] = {0, 0, 1};
	float dst_p[3] = {0, 0, 1};
	float inv_m[3][3];
	float rot_m[3][3] = {
		{cos(a),		-sin(a),	((1 - cos(a)) * x) + (sin(a) * y)},
		{sin(a),		cos(a),		(sin(a) * -x) + ((1 - cos(a)) * y)},
		{0,				0,			1}};

	CvMat rot_mat = cvMat(3, 3, TYPE, rot_m);
	CvMat inv_mat = cvMat(3, 3, TYPE, inv_m);
	CvMat src_pnt = cvMat(3, 1, TYPE, src_p);
	CvMat dst_pnt = cvMat(3, 1, TYPE, dst_p);
	IplImage *tmp = cvCloneImage(image);
	CvScalar pixel;

	cvInvert(&rot_mat, &inv_mat, CV_LU);

	for (i = 0; i < tmp->height; i++)
		for (j = 0; j < tmp->width; j++) {
			dst_p[0] = i;
			dst_p[1] = j;

			cvMatMul(&inv_mat, &dst_pnt, &src_pnt);

			src_p[0] = cvGetReal2D(&src_pnt, 0, 0);
			src_p[1] = cvGetReal2D(&src_pnt, 1, 0);

			if (src_p[0] >= 0 && src_p[0] < tmp->height - 1 &&
					src_p[1] >= 0 && src_p[1] < tmp->width - 1) {

				if (isFloat(src_p[0]) || isFloat(src_p[1]))
					pixel = interpolate(tmp, src_p[0], src_p[1]);
				else
					pixel = cvGet2D(tmp, src_p[0], src_p[1]);
			}
			else
				pixel = CV_RGB(0, 0, 0);
			cvSet2D(image, i, j, pixel);
		}

	cvReleaseImage(&tmp);
	cv_stuff_reload();
}

/**
 * Returns TRUE if f is a floating point number and FALSE otherwise.
 */
gboolean isFloat (float f) {
	if (f == floorf(f)) return FALSE;
	return TRUE;
}

/**
 * Upsamples the image by replicating pixels * factor.
 */
void cv_stuff_upsample(gboolean replication, double factor) {
	cv_stuff_check();
	if (!image || factor == 0.0) return;

	CvSize		orig_size;
	IplImage	*tmp;
	int			i,
				j,
				x = -1,		/* cached floori */
				y = -1,		/* cached floorj */
				floori,		/* new i */
				floorj;		/* new j */
	CvScalar	pixel;		/* current/cached pixel */

	orig_size = cvGetSize(image);
	tmp = cvCloneImage(image);
	image = cvCreateImage(
			cvSize(orig_size.width * factor, 
				orig_size.height * factor), 
			tmp->depth, tmp->nChannels);

	if (replication) {
		/**
		 * Loop over the destination image and gets the 
		 * corresponding intensity position from the 
		 * source image then applies it to the pixels 
		 * in the destination image.
		 */
		for(i = 0; i < image->width; i++) {
			for(j = 0; j < image->height; j++) {
				floori = floor(i / factor);
				floorj = floor(j / factor);

				/**
				 * If we're going to get the same pixel 
				 * again as the last iteration, don't do 
				 * so, use the already cached pixel.
				 */
				if (floori != x || floorj != y) {
					pixel = cvGet2D(tmp, floorj, floori);
					x = floori;
					y = floorj;
				}

				cvSet2D(image, j, i, pixel);
			}
		}
	}

	cvReleaseImage(&tmp);
	cv_stuff_reload();
}

/**
 * Upsamples the image using built-in functions in OpenCV, either 
 * by replication or interpolation depending on the boolean value 
 * given to replication.
 */
void cv_stuff_upsample_opencv(gboolean replication, double factor) {
	cv_stuff_check();
	if (!image || factor == 0.0) return;

	CvSize		orig_size;
	IplImage	*tmp;

	orig_size = cvGetSize(image);
	tmp = cvCloneImage(image);
	image = cvCreateImage(
			cvSize(orig_size.width * factor, 
				orig_size.height * factor), 
			tmp->depth, tmp->nChannels);

	if (replication)
		cvResize(tmp, image, CV_INTER_NN);
	else
		cvResize(tmp, image, CV_INTER_LINEAR);

	cvReleaseImage(&tmp);
	cv_stuff_reload();
}

/**
 * Scales the image to size (width, height) using interpolation 
 * by using built-in OpenCV functions.
 */
void cv_stuff_scale(int width, int height) {
	cv_stuff_check();
	if (!image) return;

	IplImage	*tmp;

	tmp = cvCloneImage(image);
	image = cvCreateImage(
			cvSize(width, height), 
			tmp->depth, tmp->nChannels);
	cvResize(tmp, image, CV_INTER_CUBIC);

	cvReleaseImage(&tmp);
	cv_stuff_reload();
}

