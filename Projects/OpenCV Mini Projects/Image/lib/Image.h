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

#import <objc/Object.h>
#import "Point.h"

#import <cv.h>

@interface Image: Object {
@protected
	IplImage	*img,
				*src;
}

- loadFromFile: (STR) filename;
- saveToFile: (STR) filename;

- negate;
- rotateAround: (PNT) point withAngle: (int) angle;

- revert;

- free;

@end

