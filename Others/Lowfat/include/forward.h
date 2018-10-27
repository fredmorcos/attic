////////////////////////////////////////////////////////////////////////////////
//3456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 
//
//  lowfat - an "engine" for natural document viewing for free desktop-systems
//
//  copyright (c) 2007 Mirco Müller
//
//  lowfat is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  lowfat is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Foobar; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
////////////////////////////////////////////////////////////////////////////////

#ifndef _FORWARD_H
#define _FORWARD_H

#include "shared_ptr.h"

class Action;
class Component;
class Layout;
class Image;
class Background;
class Selection;
class LfWindow;
class GuiObserver;
class PhotoObserver;

typedef SharedPtr<Action> ActionPtr;
typedef SharedPtr<Component> ComponentPtr;
typedef SharedPtr<Layout> LayoutPtr;
typedef SharedPtr<Image> ImagePtr;
typedef SharedPtr<Background> BackgroundPtr;
typedef SharedPtr<Selection> SelectionPtr;
typedef SharedPtr<LfWindow> LfWindowPtr;
typedef SharedPtr<GuiObserver> GuiObserverPtr;
typedef SharedPtr<PhotoObserver> PhotoObserverPtr;

#endif // _FORWARD_H

