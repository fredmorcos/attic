/** <title>GSWindowDecorationView</title>

   Copyright (C) 2004 Free Software Foundation, Inc.

   Author: Alexander Malmberg <alexander@malmberg.org>
   Date: 2004-03-24

   This file is part of the GNUstep GUI Library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; see the file COPYING.LIB.
   If not, write to the Free Software Foundation,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
*/

#ifndef _GNUstep_H_GSWindowDecorationView
#define _GNUstep_H_GSWindowDecorationView

#include <Foundation/NSGeometry.h>
#include "AppKit/NSView.h"

@class NSWindow;

@protocol GSWindowDecorator
- (id) newWindowDecorationViewWithFrame: (NSRect)frame
				 window: (NSWindow *)window;

- (NSRect) contentRectForFrameRect: (NSRect)aRect
			 styleMask: (unsigned int)aStyle;
- (NSRect) frameRectForContentRect: (NSRect)aRect
			 styleMask: (unsigned int)aStyle;
- (float) minFrameWidthWithTitle: (NSString *)aTitle
		       styleMask: (unsigned int)aStyle;
@end


/*
Abstract superclass for the top-level view in each window. This view is
responsible for managing window decorations. Concrete subclasses may do
this, either directly, or indirectly (by using the backend).
*/
@interface GSWindowDecorationView : NSView
{
  NSWindow *window; /* not retained */
  int windowNumber;

  NSRect contentRect;

  int inputState;
  BOOL documentEdited;
}
+ (id<GSWindowDecorator>) windowDecorator;

- (id) initWithFrame: (NSRect)frame window: (NSWindow *)w;

- (void) setBackgroundColor: (NSColor *)color;
- (void) setContentView: (NSView *)contentView;
- (void) setDocumentEdited: (BOOL)flag;
- (void) setInputState: (int)state;
- (void) setTitle: (NSString *)title;

/*
Called when the backend window is created or destroyed. When it's destroyed,
windowNumber will be 0.
*/
- (void) setWindowNumber: (int)windowNumber;

@end


/*
Standard OPENSTEP-ish window decorations.
*/
@class NSButton;

@interface GSStandardWindowDecorationView : GSWindowDecorationView
{
  BOOL hasTitleBar, hasResizeBar, hasCloseButton, hasMiniaturizeButton;
  BOOL isTitled;
  NSRect titleBarRect;
  NSRect resizeBarRect;
  NSRect closeButtonRect;
  NSRect miniaturizeButtonRect;

  NSButton *closeButton, *miniaturizeButton;
}
@end

#endif

