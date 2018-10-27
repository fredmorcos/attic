// helloWorldOpenCV.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <cv.h>
#include <highgui.h>
#include <iostream>
#include <algorithm>
using namespace std;

static IplImage* img = 0; 
static int height,width,step,channels;
static uchar *data;
static int i,j,k;


void arithScalling(IplImage* img, int x)
{

	IplImage* bw=cvCloneImage(img);

	static int height,width,step,channels;
	static uchar *data;
	static int i,j,k;

	height    = bw->height;
	width     = bw->width;
	step      = bw->widthStep;
	channels  = bw->nChannels;

	data      = (uchar *)bw->imageData;

	printf("%i",step);

	cvNamedWindow( "Original 1", CV_WINDOW_AUTOSIZE);
	cvShowImage( "Original 1", bw );

	for(i=0;i<height;i++) for(j=0;j<width;j++) for(k=0;k<channels;k++)
		data[i*step+j*channels+k]=data[i*step+j*channels+k] * x;
		

	cvNamedWindow( "Arithmetic Scalling", CV_WINDOW_AUTOSIZE);
	cvShowImage( "Arithmetic Scalling", bw );
	cvSaveImage("Test.jpg",bw);
}




int _tmain(int argc, _TCHAR* argv[])
{
	img=cvLoadImage("cartoon.png",-1);
	if(!img) printf("Could not load image file: %s\n");

	IplImage *bw = cvCreateImage(cvSize(100, 100), IPL_DEPTH_8U, 1);
	bw = cvCloneImage(img);
	int fd = bw->widthStep;
	printf("%i",fd);

	arithScalling(img,10);
	cvWaitKey(0);
	return 0;
}


