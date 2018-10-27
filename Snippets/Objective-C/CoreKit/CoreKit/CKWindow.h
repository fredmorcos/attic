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

#import <Foundation/Foundation.h>
#import "CKWidget.h"

@class CKWidget;

@interface CKWindow : CKWidget

- init;
- setTitle: (NSString *) title;
- (NSString *) title;
- setResizable: (BOOL) val;
- (BOOL) resizable;
- add: (CKWidget *) w;

@end

