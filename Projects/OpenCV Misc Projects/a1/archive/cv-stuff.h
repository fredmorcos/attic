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

#ifndef __CV_STUFF_H__
#define __CV_STUFF_H__

#include <cv.h>
#include <glib.h>

void cv_stuff_window(char *);
void cv_stuff_destroy();
void cv_stuff_reload();

void cv_stuff_save_image(char *);

void cv_stuff_negate();
CvMat *rotationMatrix (int, int, double);
void cv_stuff_rotate(int, int, double);
void cv_stuff_upsample(gboolean, double);
void cv_stuff_scale(int, int);

#endif	/* __CV_STUFF_H__ */

