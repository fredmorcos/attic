/*
	This file is part of OpenCV-Samples.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	OpenCV-Samples is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	OpenCV-Samples is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with OpenCV-Samples.  If not, see <http://www.gnu.org/licenses/>.
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
	cvShowImage("Image", image);
}

/**
 * Saves image data to a filename.
 */
void cv_stuff_save_image(char *filename) {
	if (image) cvSaveImage(filename, image);
}

/**
 * Inverts an image (gets its negative) by 
 * going over all pixels in the image data
 * and doing a val = 255 - val.
 */
void cv_stuff_negate() {
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
	 * with the image as a 1D matrix.
	 */
/*	for (i = 0; i < h; i++)
		for (j = 0; j < w; j++)
			for (k = 0; k < c; k++) {
				p = i * s + j * c + k;
				d[p] = 255 - d[p];
			}
*/
	/**
	 * Another way to get the inverse of an image,
	 * though slower. Deals with the image as a 
	 * 2D matrix.
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

void cv_stuff_rotate(int x, int y, double angle) {
	if (!image) return;

	int i, j;
	float a = angle * M_PI / 180;
	float src_p[3] = {0, 0, 1};
	float dst_p[3] = {0, 0, 1};
	float inv_m[3][3];
	float rot_m[3][3] = {
		{cos(a),		-sin(a),	((1 - cos(a)) * x) - (sin(a) * y)},
		{sin(a),		cos(a),		(sin(a) * x) + ((1 - cos(a)) * y)},
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

			if (src_p[0] > 0 && src_p[0] < tmp->height &&
					src_p[1] > 0 && src_p[1] < tmp->width)
				pixel = cvGet2D(tmp, src_p[0], src_p[1]);
			else
				pixel = CV_RGB(0, 0, 0);
			cvSet2D(image, i, j, pixel);
		}

	cvReleaseImage(&tmp);
	cv_stuff_reload();
}

gboolean isFloat (float f) {
	if (f == roundf(f)) return FALSE;
	return TRUE;
}

/*
void cv_stuff_rotate__(int x, int y, double angle) {
	if (!image) return;

	IplImage	*tmp;
	double		a;				// angle in radians
	int			i,
				j,
				w,
				h,
				point_m[3][1],
				px,
				py;
	CvMat		to_mat,			// translation to origin
				r_mat,			// rotation
				t_mat,			// translation back
				tmp_mat,		// temporary for multiplication
				res_mat,		// result matrix
				inv_mat,		// inverse matrix
				point_mat,
				pos_mat;
	float 		tmp_m[3][3],
		  		res_m[3][3],
		  		inv_m[3][3],
				pos_m[3][1];
	CvScalar	orig;

	w = image->width;
	h = image->height;
	a = angle * M_PI / 180;	// convert angle to radians

	// translation to origin
	int to_m[3][3]	= {	{1,		0,		-x},
						{0,		1,		-y},
						{0,		0,		1}};
	// rotation around origin
	float r_m[3][3]	= {	{cos(a),	-sin(a),	0},
						{sin(a),	cos(a),		0},
						{0,			0,			1}};
	// translation back to (x, y)
	int t_m[3][3] = {	{1,		0,		x},
						{0,		1,		y},
						{0,		0,		1}};

	tmp_mat = cvMat(3, 3, TYPE, tmp_m);
	res_mat = cvMat(3, 3, TYPE, res_m);
	inv_mat = cvMat(3, 3, TYPE, inv_m);

	to_mat = cvMat(3, 3, TYPE, to_m);
	r_mat = cvMat(3, 3, TYPE, r_m);
	t_mat = cvMat(3, 3, TYPE, t_m);

	cvMatMul(&t_mat, &r_mat, &tmp_mat);
	cvMatMul(&tmp_mat, &to_mat, &res_mat);

	cvInvert(&res_mat, &inv_mat, CV_LU);

	pos_mat = cvMat(3, 1, TYPE, pos_m);
	tmp = cvCloneImage(image);
	for(i = 0; i < h; i++)
		for(j = 0; j < w; j++) {
			point_m[0][0] = i;
			point_m[1][0] = j;
			point_m[2][0] = 0;

			point_mat = cvMat(3, 1, TYPE, point_m);
			cvMatMul(&inv_mat, &point_mat, &pos_mat);

			px = (int)cvmGet(&pos_mat, 0, 0);
			py = (int)cvmGet(&pos_mat, 1, 0);

			if (px >= 0 && px < w && py >= 0 && py < h) {
				orig = cvGet2D(tmp, (int)cvmGet(&pos_mat, 1, 0), (int)cvmGet(&pos_mat, 0, 0));
			}
			else {
				orig = CV_RGB(0, 0, 0);
			}
			cvSet2D(image, i, j, orig);
		}

	cvReleaseImage(&tmp);
	cv_stuff_reload();
}
*/
/*
CvMat *rotationMatrix (int x, int y, double angle) {
	double	a = angle * M_PI / 180;
	double	matrix[2][3];
	CvMat	tmp = cvMat(2, 3, TYPE, matrix);
	CvMat	*res = cvCreateMat(2, 3, TYPE);
	
	matrix[0][0] = cos(a);
	matrix[0][1] = sin(a);
	matrix[0][2] = ((1 - cos(a)) * x) - (sin(a) * y);
	matrix[1][0] = -sin(a);
	matrix[1][1] = cos(a);
	matrix[1][2] = (sin(a) * x) + ((1 - cos(a)) * y);

	cvConvertScale(&tmp, res, 1, 0);
	return res;
}
*/

/**
 * Rotates an image around point (x, y) by angle. Uses 
 * OpenCV built-in functions to do so. Unused.
 */
/*
void cv_stuff_rotate_(int x, int y, double angle) {
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
*/

/**
 * Upsamples the image using built-in functions in OpenCV, either 
 * by replication or interpolation depending on the boolean value 
 * given to replication.
 */
void cv_stuff_upsample(gboolean replication, double factor) {
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

