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

#import <AppKit/AppKit.h>
#import "GridProperties.h"

@interface PreferencesController: NSObject {
	NSColorWell		*gridGridColorWell;
	NSTextField		*gridGridSpacingField,
					*gridSnapDistanceField;
	NSButton		*gridShowGridButton,
					*gridSnapToGridButton;
					
	NSPopUpButton	*preferencesPopUp;
	NSWindow		*preferencesWindow;
	
	GridProperties	*gridProperties;
}

- reloadGridProperties;

- (void) gridGridColorChanged: (id) sender;
- (void) gridGridSpacingChanged: (id) sender;
- (void) gridShowGridChanged: (id) sender;
- (void) gridSnapToGridChanged: (id) sender;
- (void) gridSnapDistanceChanged: (id) sender;

- (void) preferencesPopUpChanged: (id) sender;

@end

