/*
	This file is part of cv1-gtk-cairo.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	cv1-gtk-cairo is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cv1-gtk-cairo is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cv1-gtk-cairo.  If not, see <http://www.gnu.org/licenses/>.
*/

#import "Window.h"
#import "Image.h"

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

