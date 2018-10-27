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

#ifndef __UI_BUILDER_H__
#define __UI_BUILDER_H__

#import <objc/Object.h>
#import <gtk/gtk.h>

@interface UIBuilder: Object {
@protected
	GtkBuilder *builder;
}

- init;
- 
- release;

GtkWidget		*ui_get_widget(gchar *);
GtkAction		*ui_get_action(gchar *);

#endif	/* __UI_BUILDER_H__ */

