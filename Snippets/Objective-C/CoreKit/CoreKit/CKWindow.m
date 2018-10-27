/*
 *	This file is part of CoreKit.
 *
 *	Copyright 2009 Fred Morcos <fred.morcos@gmail.com>
 *
 *	CoreKit is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	CoreKit is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with CoreKit.  If not, see <http://www.gnu.org/licenses/>.
 */

#import "CKWindow.h"

@implementation CKWindow

- init {
	[super init];
	widget = clutter_stage_get_default();
	return self;
}

- setTitle: (NSString *) title {
	clutter_stage_set_title (CLUTTER_STAGE(widget), [title cString]);
	return self;
}

- (NSString *) title {
	return [NSString stringWithCString: 
		clutter_stage_get_title(CLUTTER_STAGE(widget))];
}

- setResizable: (BOOL) val {
	clutter_stage_set_user_resizable(CLUTTER_STAGE(widget), val);
	return self;
}

- (BOOL) resizable {
	return clutter_stage_get_user_resizable(CLUTTER_STAGE(widget));
}

- add: (CKWidget *) w {
	clutter_container_add(CLUTTER_CONTAINER(widget), [w widget], NULL);
	return self;
}

@end

