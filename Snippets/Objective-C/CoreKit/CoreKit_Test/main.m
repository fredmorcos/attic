/*	
 *	This file is part of OpenGrafik.
 *
 *	Copyright 2009 Fred Morcos <fred.morcos@gmail.com>
 *
 *	OpenGrafik is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	OpenGrafik is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OpenGrafik.  If not, see <http://www.gnu.org/licenses/>.
 */

#import <CoreKit/CoreKit.h>

int main (int argc, char **argv) {
	CREATE_AUTORELEASE_POOL (pool);

	[CoreKit init: argc: argv];

	NSString *svgFile = [[NSBundle mainBundle] pathForResource: @"testimg" ofType: @"svg"];
	CKWindow *win = [[CKWindow new] setResizable: YES];
	CKSvgImage *img = [[CKSvgImage alloc] initFromFile: svgFile withWidth: 80 height: 80];
	[win add: img];
	[win showAll];

	[CoreKit main];

	RELEASE (pool);

	return 0;
}

