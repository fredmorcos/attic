/*	
 *	This file is part of OpenGrafik.
 *
 *	Copyright 2009 Michael Morckos <mikey.morckos@gmail.com>
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
#import "DiagramView.h"

@interface Document: NSDocument {
	Diagram		*model;
	DiagramView	*view;
}

@end

