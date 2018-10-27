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

#import "Image.h"

#import <cv.h>

@implementation Image

- loadFromFile: (STR) filename {
	src = cvLoadImage(filename, -1);
	img = cvCloneImage(src);
}

- saveToFile: (STR) filename {
	if (img) cvSaveImage(filename, img);
}

- negate {
	if (!img) return;

	for (int i = 0; i < img->height * img->width * img->nChannels; i++)
		img->imageData[i] = 255 - img->imageData[i];
}

- rotateAround: (PNT) point withAngle: (int) angle {
	if (!img) return;


}

- revert {
	cvReleaseImage(&img);
	img = cvCloneImage(src);
}

- free {
	cvReleaseImage(&img);
	cvReleaseImage(&src);

	[super free];
}

@end

