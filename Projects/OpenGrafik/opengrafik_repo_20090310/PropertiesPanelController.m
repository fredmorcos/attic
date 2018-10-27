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

#import "PropertiesPanelController.h"
#import "config.h"
#import <Renaissance/Renaissance.h>

@implementation PropertiesPanelController

- init {
	self = [super init];
	[NSBundle loadGSMarkupNamed: @"PropertiesPanel" owner: self];
	[self initCategoriesPopup];
	[self resetPanelFrame];
	return self;
}

// FIXME should retrieve property categories from DiagramBaseObject, 
// DiagramObject and DiagramConnector classes?
- initCategoriesPopup {
	[categoriesPopup addItemWithTitle: @"Size"];
	[categoriesPopup addItemWithTitle: @"Position"];
	[categoriesPopup addItemWithTitle: @"Colors"];
	return self;
}

- resetPanelFrame {
	NSRect	screenFrame,
			panelFrame;

	screenFrame = [[panel screen] frame];
	panelFrame = [panel frame];

	panelFrame.origin.x = 
		screenFrame.size.width - panelFrame.size.width - SPACING;
	panelFrame.origin.y = SPACING;
	
	[panel setFrame: panelFrame display: YES animate: NO];
	
	return self;
}

@end

