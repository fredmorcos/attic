#import "Window.h"
#import "Image.h"
#import <highgui.h>

@implementation Window

/**
 * Destroys all OpenCV windows.
 */
+ destroyAll {
	cvDestroyAllWindows();
}

/**
 * Window constructor with name.
 */
- initWithName: (char *) name {
	windowName = name;
	cvNamedWindow(windowName, CV_WINDOW_AUTOSIZE);
	return self;
}

/**
 * Show image in window.
 */
- showImage: (Image *) image {
	cvShowImage(windowName, [image image]);
	return self;
}

/**
 * Window destructor.
 */
- free {
	cvDestroyWindow(windowName);
	return [super free];
}

@end

