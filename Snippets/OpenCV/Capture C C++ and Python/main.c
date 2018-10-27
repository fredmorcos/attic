#include <stdio.h>
#include <stdlib.h>
#include <opencv/cv.h>
#include <opencv/cxcore.h>
#include <opencv/highgui.h>

int main (int argc, char **argv)
{
  CvCapture *capture = cvCaptureFromCAM (CV_CAP_ANY);
  IplImage *frame, *edges;

  if (!capture)
    {
      fprintf (stderr, "Error: capture is NULL\n");
      return EXIT_FAILURE;
    }

  cvNamedWindow ("window1", CV_WINDOW_AUTOSIZE);
  cvNamedWindow ("window2", CV_WINDOW_AUTOSIZE);

  while (1)
    {
      frame = cvQueryFrame (capture);

      if (!frame)
	{
	  fprintf (stderr, "Error: frame is NULL\n");
	  return EXIT_FAILURE;
	}

      cvShowImage ("window1", frame);

      edges = cvCreateImage (cvSize (frame->width, frame->height),
			     frame->depth, frame->nChannels);

      cvSobel (frame, edges, 1, 1, 3);
      /* cvCanny (frame, edges, 3, 3, 3); */
      /* cvSmooth (frame, edges, CV_GAUSSIAN, 15, 0, 0, 0); */

      if (!edges)
	{
	  fprintf (stderr, "Error: edges is NULL\n");
	  return EXIT_FAILURE;
	}

      cvShowImage ("window2", edges);

      if ((cvWaitKey (10) & 255) == 27)
	break;
    }

  cvReleaseCapture (&capture);
  cvDestroyWindow ("window1");

  return EXIT_SUCCESS;
}
