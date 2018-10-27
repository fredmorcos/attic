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

#import "ShapesPanelController.h"
#import "config.h"
#import <Renaissance/Renaissance.h>

@implementation ShapesPanelController

- init {
	self = [super init];
	[NSBundle loadGSMarkupNamed: @"ShapesPanel" owner: self];
	[self initCategoriesPopup];
	[self resetPanelFrame];
	return self;
}

// FIXME should load categories and shapes from NSBundle!
- initCategoriesPopup {
	[categoriesPopup addItemWithTitle: @"Normal"];
	[categoriesPopup addItemWithTitle: @"UML"];
	[categoriesPopup addItemWithTitle: @"ERD"];
	return self;
}

- resetPanelFrame {
	NSRect	screenFrame,
			panelFrame;

	screenFrame = [[panel screen] frame];
	panelFrame = [panel frame];

	panelFrame.origin.x = 
		screenFrame.size.width - panelFrame.size.width - SPACING;
	panelFrame.origin.y =
		screenFrame.size.height - panelFrame.size.height - SPACING;
	
	[panel setFrame: panelFrame display: YES animate: YES];
	
	return self;
}

@end

