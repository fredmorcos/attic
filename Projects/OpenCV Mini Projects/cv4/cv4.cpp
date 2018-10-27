/*
	This file is part of cv4.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	cv4 is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cv4 is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cv4.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <opencv/highgui.h>
#include <opencv/cxcore.h>
#include <opencv/cv.h>

/* Enumeration for direction. */
typedef enum _Orientation {
	NONE, NORTH, WEST, SOUTH, EAST
} Orientation;

/* Struct to represent movement (distance and direction in x and y). */
typedef struct _Movement {
	int x, y;
	Orientation x_orient, y_orient;
} Movement;

/* Function that returns the square of x. */
double squared (double x) {
	return pow (x, 2);
}

/* Function that returns the absolute value of x. */
double absolute (double x) {
	return abs (x);
}

/* Returns the GrayScale value of pixel (x, y) in image img. */
unsigned char getGSVal (IplImage *img, int x, int y) {
	return (unsigned char) (cvGet2D (img, y, x)).val[0];
}

/* Sets the GrayScale value of pixel (x, y) to val in image img. */
void setGSVal (IplImage *img, int x, int y, unsigned char val) {
	cvSet2D (img, y, x, cvScalar ((double) val));
}

/* 
 * Runs the SXD algorithm on images img1 and img2 comparing pixels (x1, y1) and
 * (x2, y2) along with their neighborhoods of size neigh. The correlation 
 * function is passed as to either use squared() or absolute().
 */
double sxd_pixel (IplImage *img1, IplImage *img2,
					int x1, int y1, int x2, int y2,
					int neigh, double (* corr_func) (double)) {
	double res = 0.0;
	for (int i = -neigh; i <= neigh; i++)
		for (int j = -neigh; j <= neigh; j++)
			res += corr_func ((double) (getGSVal(img1, x1 + i, y1 + j) -
										getGSVal(img2, x2 + i, y2 + j)));
	return res;
}

/*
 * Runs the SXD algorithm on pixel (x, y) in image img1 by looping over all of
 * its candidates of area around the pixel with size cand in image img2 using a 
 * kernel of neighborhood size kern and a correlation function corr_fun. Returns 
 * the amount of movement in x and y and their corresponding directions.
 */
Movement *sxd (IplImage *img1, IplImage *img2,
			   int x, int y, int kern,
			   int cand, double (* corr_fun) (double)) {
	int			min_sxd_x = 0,
				min_sxd_y = 0;
	double		min_sxd = 0.0,
				tmp_sxd = 0.0;
	bool		first = true;
	Movement	*res = (Movement *) malloc(sizeof(Movement));
   
	for (int i = -cand; i <= cand; i++) {
		for (int j = -cand; j <= cand; j++) {
			tmp_sxd = sxd_pixel(img1, img2, x, y, x + i, y + j, kern, corr_fun);
			if (first) {
				min_sxd = tmp_sxd;
				min_sxd_x = x + i;
				min_sxd_y = y + j;
				first = false;
			} else {
				if (min_sxd > tmp_sxd) {
					min_sxd = tmp_sxd;
					min_sxd_x = x + i;
					min_sxd_y = y + j;
				}
			}
		}
	}
   
	res->x = min_sxd_x - x;
	res->y = min_sxd_y - y;

	if (res->x == 0)
		res->x_orient = NONE;
	else if (res->x > 0)
		res->x_orient = EAST;
	else if (res->x < 0)
		res->x_orient = WEST;
	res->x = abs(res->x);

	if (res->y == 0)
		res->y_orient = NONE;
	else if (res->y > 0)
		res->y_orient = SOUTH;
	else if (res->y < 0)
		res->y_orient = NORTH;
	res->y = abs(res->y);

	return res;
}

/* Returns a string corresponding to the Orientation. */
char *getString (Orientation orient) {
	if (orient == NONE) return (char *) "None";
	else if (orient == NORTH) return (char *) "North";
	else if (orient == WEST) return (char *) "West";
	else if (orient == SOUTH) return (char *) "South";
	else if (orient == EAST) return (char *) "East";
	return (char *) "Unknown";
}

/* Question 1. Calls the SXD algorithm after loading the images. */
void q1() {
	IplImage	*img1 = cvLoadImage("guc1_1058.bmp", CV_LOAD_IMAGE_GRAYSCALE),
				*img2 = cvLoadImage("guc2_1059.bmp", CV_LOAD_IMAGE_GRAYSCALE);
	Movement	*ssd_mov = sxd(img1, img2, 36, 36, 2, 5, &squared),
				*sad_mov = sxd(img1, img2, 36, 36, 2, 5, &absolute);
	printf("SSD: In X: %i to the %s, Y: %i to the %s\n"
		   "SAD: In X: %i to the %s, Y: %i to the %s\n",
		   ssd_mov->x, getString(ssd_mov->x_orient),
		   ssd_mov->y, getString(ssd_mov->y_orient),
		   sad_mov->x, getString(sad_mov->x_orient),
		   sad_mov->y, getString(sad_mov->y_orient));
	cvNamedWindow("Image 1", CV_WINDOW_AUTOSIZE);
	cvNamedWindow("Image 2", CV_WINDOW_AUTOSIZE);
	cvShowImage("Image 1", img1);
	cvShowImage("Image 2", img2);
	cvReleaseImage(&img1);
	cvReleaseImage(&img2);
	free(ssd_mov);
	free(sad_mov);
}

#define LINES		8   /* number of matching points (lines) in file */
#define FIRST_IMG   0   /* index of Image 1 */
#define SECND_IMG   1   /* index of Image 2 */

/*
 * Calculates and shows the epipolar line on dstImg from the point (x, y) in a 
 * source image. Reads the matching points from file "points.txt", calculates 
 * the fundamental matrix between points1 and points, multiplies the 
 * fundamental matrix with the the (x, y, 1) matrix of the source point to 
 * result in the equation of the epipolar line (a, b, c) -> ax + by + c = 0.
 * To draw the line, we equate x to 0 and calculate its resulting y, then 
 * equate x to 0 and calculate the resulting y, resulting in the source point of 
 * the line, then we equate x to image width and calculate y and the y to image 
 * width and calculate x, resulting in the final point of the line.
 */
void showEpipolarLine (IplImage *dstImg, int x, int y, short imgNum) {
	double  lineArr[3][1],
			pts1[LINES][2],
			pts2[LINES][2],
			a, b, c,
			ipPntArr[][1] = {{x}, {y}, {1}},
			fundArr[3][3];
	float	tmp_1_0, tmp_1_1, tmp_2_0, tmp_2_1;
	CvMat   lineMat = cvMat (3, 1, CV_64FC1, lineArr),
			fundMat = cvMat (3, 3, CV_64FC1, fundArr),
			ipPntMat = cvMat (3, 1, CV_64FC1, ipPntArr),
			points1 = cvMat (LINES, 2, CV_64FC1, pts1),
			points2 = cvMat (LINES, 2, CV_64FC1, pts2);
	FILE	*file = fopen("points.txt", "r");
	
	for (int i = 0; i < LINES; i++) {
		fscanf(file, "%f,%f,%f,%f", &tmp_1_0, &tmp_1_1, &tmp_2_0, &tmp_2_1);
		pts1[i][0] = (double) tmp_1_0;
		pts1[i][1] = (double) tmp_1_1;
		pts2[i][0] = (double) tmp_2_0;
		pts2[i][1] = (double) tmp_2_1;
	}
	fclose(file);
	
	if (imgNum == FIRST_IMG)
		cvFindFundamentalMat (&points1, &points2, &fundMat);
	else
		cvFindFundamentalMat (&points2, &points1, &fundMat);
	cvMatMul (&fundMat, &ipPntMat, &lineMat);
	
	a = cvmGet (&lineMat, 0, 0);
	b = cvmGet (&lineMat, 1, 0);
	c = cvmGet (&lineMat, 2, 0);
	cvLine (dstImg, cvPoint (0, -c / b), cvPoint(-c / a, 0), cvScalar(1));
	cvLine (dstImg, cvPoint (dstImg->width, ((-a * dstImg->width) - c) / b), 
					cvPoint (0, -c / b), cvScalar(1));
	
	if (imgNum == FIRST_IMG)
		cvShowImage("Image 2", dstImg);
	else
		cvShowImage("Image 1", dstImg);
}

/* 
 * Calls the function showEpipolarLine when Image 1 is clicked on to show the 
 * line on Image 2.
 */
void img1_clicked (int event, int x, int y, int flags, void *param) {
	if (event == CV_EVENT_LBUTTONDBLCLK)
		showEpipolarLine((IplImage *) param, x, y, FIRST_IMG);
}

/* 
 * Calls the function showEpipolarLine when Image 2 is clicked on to show the 
 * line on Image 1.
 */
void img2_clicked (int event, int x, int y, int flags, void *param) {
	if (event == CV_EVENT_LBUTTONDBLCLK)
		showEpipolarLine((IplImage *) param, x, y, SECND_IMG);
}

/* Question 2. Loads the images and connects the mouse callbacks. */
void q2() {
	IplImage	*img1 = cvLoadImage("painting1_1060.jpg",
									CV_LOAD_IMAGE_GRAYSCALE),
				*img2 = cvLoadImage("painting2_1061.jpg",
									CV_LOAD_IMAGE_GRAYSCALE);
	cvNamedWindow("Image 1", CV_WINDOW_AUTOSIZE);
	cvNamedWindow("Image 2", CV_WINDOW_AUTOSIZE);
	cvSetMouseCallback("Image 1", img1_clicked, (void *) img2);
	cvSetMouseCallback("Image 2", img2_clicked, (void *) img1);
	cvShowImage("Image 1", img1);
	cvShowImage("Image 2", img2);
}

/* Traverses and matrix and prints its data. */
void printMatrix (CvMat mat) {
	for (int i = 0; i < mat.rows; i++) {
		for (int j = 0; j < mat.cols; j++) {
			printf("%f\t", cvmGet(&mat, i, j));
		}
		printf("\n");
	}
	printf("\n");
}

/* Returns the rotation matrix from the p, t and s matrices. */
CvMat getRotationMatrix (int pan, int tilt, int swing) {
	double  p = pan * 3.14 / 180.0,
			t = tilt * 3.14 / 180.0,
			s = swing * 3.14 / 180.0,
			arr1[3][3] = {  {cos(s),			sin(s),			0},
							{-1.0 * sin(s),		cos(s),			0},
							{0,					0,				1}},
			arr2[3][3] = {  {1,					0,				0},
							{0,					cos(t),			sin(t)},
							{0,					-1.0 * sin(t),	cos(t)}},
			arr3[3][3] = {  {cos(p),			sin(p),			0},
							{-1.0 * sin(p),		cos(p),			0},
							{0,					0,				1}},
			tmpArr[3][3] = { {0,0,0},	{0,0,0},	{0,0,0}},
			rArr[3][3] = { {0,0,0},	{0,0,0},	{0,0,0}};
	CvMat   mat1 = cvMat(3, 3, CV_64FC1, arr1),
			mat2 = cvMat(3, 3, CV_64FC1, arr2),
			mat3 = cvMat(3, 3, CV_64FC1, arr3),
			tmpMat = cvMat(3, 3, CV_64FC1, tmpArr),
			rMat = cvMat(3, 3, CV_64FC1, rArr);
	
	cvMatMul(&mat1, &mat2, &tmpMat);
	cvMatMul(&tmpMat, &mat3, &rMat);
	
	return rMat;
}

/*
 * Question 4. Creates the required matrices, performs multiplication, scans
 * the result image and gets the source points using the homography matrix.
 */
void q4(int resW, int resH, int aU, int aV, int pan, int tilt, int swing,
		int camX, int camY, int camZ) {
	printf("width\t%d\nheight\t%d\n", resW, resH);
	printf("alpha_U\t%d\nalpha_V\t%d\n", aU, aV);
	printf("pan\t%d\ntilt\t%d\nswing\t%d\n", pan, tilt, swing);
	printf("cam_X\t%d\ncam_Y\t%d\ncam_Z\t%d\n", camX, camY, camZ);
	
	IplImage	*img1 = cvLoadImage("cedfence_1282.bmp",
									CV_LOAD_IMAGE_GRAYSCALE),
				*img2 = cvCreateImage(cvSize(resW, resH), 8, img1->nChannels);
	double		aArr[3][3] =	{	{aU,	0,		(double) (resW / 2.0)},
								{0,		aV,		(double) (resH / 2.0)},
								{0,		0,		1}},
				tArr[3][1] = {  {camX}, {camY}, {camZ}},
				arArr[3][3] = { {0,0,0},	{0,0,0},	{0,0,0}},
				artArr[3][1] =	{{0},	    {0},		{0}},
				hArr[3][3] = {  {0,0,0},  {0,0,0},  {0,0,0}},
				ihArr[3][3] = {  {0,0,0},  {0,0,0},  {0,0,0}},
				pntArr[3][1] = {{0},{0},{1}},
				dstpntArr[3][1] = {{0},{0},{1}},
				x, y, z;
	CvMat		aMat = cvMat(3, 3, CV_64FC1, aArr),
				rMat = getRotationMatrix(pan, tilt, swing),
				tMat = cvMat(3, 1, CV_64FC1, tArr),
				arMat = cvMat(3, 3, CV_64FC1, arArr),
				artMat = cvMat(3, 1, CV_64FC1, artArr),
				hMat = cvMat(3, 3, CV_64FC1, hArr),
				ihMat = cvMat(3, 3, CV_64FC1, ihArr),
				pntMat = cvMat(3, 3, CV_64FC1, pntArr),
				dstpntMat = cvMat(3, 3, CV_64FC1, dstpntArr);
			
	/* get the a * r and a * r * t */
	cvMatMul(&aMat, &rMat, &arMat);
	cvMatMul(&arMat, &tMat, &artMat);
			
	for (int i = 0; i < arMat.rows; i++)
		for (int j = 0; j < arMat.cols; j++)
			cvmSet(&hMat, i, j, cvmGet(&arMat, i, j));
	
	for (int i = 0; i < artMat.rows; i++)
		cvmSet(&hMat, i, 2, -1.0 * cvmGet(&artMat, i, 0));
	
	cvInvert(&hMat, &ihMat);
			
	for (int i = 0; i < img2->width; i++) {
		for (int j = 0; j < img2->height; j++) {
			cvmSet(&pntMat, 0, 0, i);
			cvmSet(&pntMat, 1, 0, j);
			cvmSet(&pntMat, 2, 0, 1);
			cvMatMul(&ihMat, &pntMat, &dstpntMat);
			x = cvmGet(&dstpntMat, 0, 0);
			y = cvmGet(&dstpntMat, 1, 0);
			z = cvmGet(&dstpntMat, 2, 0);
			x = round(x / z);
			y = round(y / z);
			
			if (x < 0 || x >= img1->width || y < 0 || y >= img1->height)
				cvSet2D(img2, j, i, cvScalar(0));
			else
				cvSet2D(img2, j, i, cvGet2D(img1, y, x));
		}
	}
	
	printMatrix(hMat);
	printMatrix(ihMat);
			
	cvNamedWindow("Image 1", CV_WINDOW_AUTOSIZE);
	cvNamedWindow("Image 2", CV_WINDOW_AUTOSIZE);
	cvShowImage("Image 1", img1);
	cvShowImage("Image 2", img2);
}

/*
 * Program starts here. http://www.geekherocomic.com/2008/08/18/int-mainvoid/
 */
int main(int argc, char *argv[]) {
	if (argc == 1) {
		printf("Usage: ./cv4 [q1 | q2 | q4]\n");
		goto leave;
	} else {
		if (strcmp(argv[1], "q1") == 0) q1();
		if (strcmp(argv[1], "q2") == 0) q2();
		if (strcmp(argv[1], "q4") == 0) {
			if (argc < 12) {
				printf("Usage: ./cv4 q4 res_width res_height alpha_u alpha_v ");
				printf("pan tilt swing cam_x cam_y cam_z\n");
				goto leave;
			}
			q4(atoi(argv[2]), atoi(argv[3]), atoi(argv[4]), atoi(argv[5]), 
			   atoi(argv[6]), atoi(argv[7]), atoi(argv[8]), atoi(argv[9]), 
			   atoi(argv[10]), atoi(argv[11]));
		}
		cvWaitKey(0);
		goto free;
	}
	
	free:
		cvDestroyAllWindows();
	leave:
		return 0;
}
