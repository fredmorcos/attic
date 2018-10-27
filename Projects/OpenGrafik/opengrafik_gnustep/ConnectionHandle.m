/*	
 *	This file is part of OpenGrafik.
 *
 *	Copyright 2009	Michael Morckos <mikey.morckos@gmail.com>
 *					Frederic Morcos <fred.morcos@gmail.com>
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
 
#import "ConnectionHandle.h"
 
@implementation ConnectionHandle

- init {
	self = [super init];
	coupledHandles = [NSMutableArray new];
	return self;
}

- (void) dealloc {
	RELEASE(coupledHandles);
	[super dealloc];	
}		

- initColors {
	[self setBackgroundColor: [NSColor colorWithCalibratedRed: 0.7 
					   green: 0.1 blue: 0.1 alpha: 0.8]];
	[self setForegroundColor: [NSColor colorWithCalibratedRed: 0.5
					   green: 0.1 blue: 0.1 alpha: 1.0]];
	return self;
}

- coupleHandle: (ConnectionHandle *) handle {
	[coupledHandles addObject: handle];	
	return self;		
}

- decoupleHandle: (ConnectionHandle *) handle {
	[coupledHandles removeObject: handle];	
	return self;
}	

@end

