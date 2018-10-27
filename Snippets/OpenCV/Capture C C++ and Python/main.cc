#include <iostream>
#include <cv.h>
#include <highgui.h>

using namespace std;
using namespace cv;

int main(int argc, char **argv)
{
  VideoCapture capture(0);

  Mat frame, edges;

  namedWindow("edges", 1);

  while(1)
    {
      capture >> frame;
      cvtColor(frame, edges, CV_BGR2GRAY);
      GaussianBlur(edges, edges, Size(7,7), 1.5, 1.5);
      Canny(edges, edges, 0, 30, 3);

      imshow("edges", edges);
      if (waitkey(30) >= 0)
	break;
    }

  return 0;
}
