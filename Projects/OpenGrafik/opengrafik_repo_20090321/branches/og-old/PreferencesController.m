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

#import "PreferencesController.h"
#import "NSView+Extensions.h"

@implementation PreferencesController

- awakeFromNib {
	[preferencesPopUp removeAllItems];
	[preferencesPopUp addItemWithTitle: @"Grid"];

	gridProperties = [GridProperties sharedGridProperties];
	[self reloadGridProperties];
	[[NSNotificationCenter defaultCenter] 
		addObserver: self selector: @selector(reloadGridProperties) 
		name: @"OGGridVisualPropertyChanged" object: gridProperties];
	return self;
}

- reloadGridProperties {
	// FIXME segfaults
	// [gridGridColorWell setColor: [gridProperties color]];
	[gridGridSpacingField setStringValue: 
		[NSString stringWithFormat: @"%d", [gridProperties spacing]]];
	[gridSnapDistanceField setStringValue: 
		[NSString stringWithFormat: @"%d", [gridProperties snapDistance]]];
	[gridShowGridButton setState: 
		[gridProperties visible] == YES ? NSOnState : NSOffState];
	[gridSnapToGridButton setState: 
		[gridProperties snap] == YES ? NSOnState : NSOffState];
	return self;
}

- (void) gridGridColorChanged: (id) sender {
	[gridProperties setColor: [sender color]];
}

- (void) gridGridSpacingChanged: (id) sender {
	[gridProperties setSpacing: [[sender stringValue] intValue]];
}

- (void) gridShowGridChanged: (id) sender {
	[gridProperties setVisible: [sender state] == NSOnState ? YES : NO];
}

- (void) gridSnapToGridChanged: (id) sender {
	[gridProperties setSnap: [sender state] == NSOnState ? YES : NO];
}

- (void) gridSnapDistanceChanged: (id) sender {
	[gridProperties setSnapDistance: [[sender stringValue] intValue]];
}

- (void) preferencesPopUpChanged: (id) sender {
	NSString *title = [sender titleOfSelectedItem];

	if ([title localizedCaseInsensitiveCompare: @"Grid"] == NSOrderedSame)
		[[preferencesWindow contentView] moveSubviewToFront: 
			[gridGridColorWell superview]];
}

@end

