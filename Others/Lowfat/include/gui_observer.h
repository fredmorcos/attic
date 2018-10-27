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

#ifndef _GUIOBSERVER_H
#define _GUIOBSERVER_H

#include <list>

#include "include/key.h"
#include "include/forward.h"

class Component;

namespace MouseButton
{
	enum
	{
		left = 1,
		right,
		mid,
		wheelUp,
		wheelDown
	};
}

struct MouseEvent
{
	Component* source;
	int x;
	int y;
	int button;
};

struct KeyEvent
{
	Component* source;
	int key;
};

struct TimeEvent
{
	Component* source;
   	unsigned int time;
};

class GuiObserver
{
	public:
		virtual ~GuiObserver();

		virtual bool onMousePress (const MouseEvent& e)
		{
			return true;
		};

		virtual bool onMouseRelease (const MouseEvent& e)
		{
			return true;
		};

		virtual bool onMouseMove (const MouseEvent& e)
		{
			return true;
		};

		virtual bool onKeyPress (const KeyEvent& e)
		{
			return true;
		};

		virtual bool onKeyRelease (const KeyEvent& e)
		{
			return true;
		};

		virtual bool onTime (const TimeEvent& e)
		{
			return true;
		};
};

class GuiObserverSubject
{
	public:
		GuiObserverSubject ();
		~GuiObserverSubject ();

		void addGuiObserver (GuiObserverPtr observer);
		void removeGuiObserver (GuiObserverPtr observer);
		void updateMousePress (const MouseEvent& e);
		void updateMouseRelease (const MouseEvent& e);
		void updateMouseMove (const MouseEvent& e);
		void updateKeyPress (const KeyEvent& e);
		void updateKeyRelease (const KeyEvent& e);
		void updateTime (const TimeEvent& e);

	private:
		std::list<GuiObserverPtr> observerList_;
};

#endif // _GUIOBSERVER_H

