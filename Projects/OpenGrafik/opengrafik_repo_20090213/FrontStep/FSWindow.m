/*
 *	This file is part of FrontStep.
 *
 *	Copyright 2009 Fred Morcos <fred.morcos@gmail.com>
 *
 *	FrontStep is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	FrontStep is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with FrontStep.  If not, see <http://www.gnu.org/licenses/>.
 */

#import "FSWindow.h"

@implementation FSWindow

- init {
	[super init];
	widget = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	return self;
}

- setTitle: (NSString *) title {
	gtk_window_set_title (GTK_WINDOW (widget), [title cString]);
	return self;
}

- (NSString *) title {
	return [NSString stringWithCString: gtk_window_get_title (GTK_WINDOW (widget))];
}

- show {
	gtk_widget_show (widget);
	return self;
}

- hide {
	gtk_widget_hide (widget);
	return self;
}

- showAll {
	gtk_widget_show_all (widget);
	return self;
}

- hideAll {
	gtk_widget_hide_all (widget);
	return self;
}

@end

