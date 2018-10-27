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

#import "DiagramModel.h"
#import "DiagramObject.h"
#import "DiagramConnector.h"

@implementation DiagramModel

- init {
	self = [super init];
	objects = [NSMutableArray array];
	connectors = [NSMutableArray array];
	[[NSNotificationCenter defaultCenter] addObserver: self 
											 selector: @selector(notifyChange:)
											 	 name: @"change"
											   object: nil];
	return self;
}

- add: (id) obj {
	BOOL	isObject = [obj isKindOfClass: [DiagramObject class]],
			isConnector = [obj isKindOfClass: [DiagramConnector class]];

	if (isObject)
		[objects addObject: obj];
	else if (isConnector)
		[connectors addObject: obj];
	else
		NSAssert(isObject || isConnector,
			 	 @"Trying to add an object to a DiagramDocument that is neither \
				 a DiagramObject nor a DiagramConnector");
	return self;
}

- remove: (id) obj {
	BOOL	isObject = [obj isKindOfClass: [DiagramObject class]],
			isConnector = [obj isKindOfClass: [DiagramConnector class]];

	if (isObject)
		[objects removeObject: obj];
	else if (isConnector)
		[connectors removeObject: obj];
	else
		NSAssert(isObject || isConnector,
			 	 @"Trying to remove an object from a DiagramDocument that is \
			 	 neither a DiagramObject nor a DiagramConnector");
	return self;
}

@end

