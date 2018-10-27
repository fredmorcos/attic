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

#include "include/gui_observer.h"

GuiObserver::~GuiObserver ()
{
}

void
GuiObserverSubject::addGuiObserver (GuiObserverPtr observer)
{
	observerList_.push_back (observer);
}

void
GuiObserverSubject::removeGuiObserver (GuiObserverPtr observer)
{
	observerList_.remove (observer);
}

void
GuiObserverSubject::updateMousePress (const MouseEvent& e)
{
	for (std::list<GuiObserverPtr>::iterator it = observerList_.begin ();
	    it != observerList_.end ();
	    it++)
		if (!(*it)->onMousePress (e))
			return;
}

void
GuiObserverSubject::updateMouseRelease (const MouseEvent& e)
{
	for (std::list<GuiObserverPtr>::iterator it = observerList_.begin ();
	     it != observerList_.end ();
	     it++)
		if (!(*it)->onMouseRelease (e))
			return;
}

void
GuiObserverSubject::updateMouseMove (const MouseEvent& e)
{
	for (std::list<GuiObserverPtr>::iterator it = observerList_.begin ();
	     it != observerList_.end ();
	     it++)
		if (!(*it)->onMouseMove (e))
			return;
}

GuiObserverSubject::GuiObserverSubject ()
{
}

GuiObserverSubject::~GuiObserverSubject ()
{
}

void
GuiObserverSubject::updateKeyPress (const KeyEvent& e)
{
	for (std::list<GuiObserverPtr>::iterator it = observerList_.begin ();
	     it != observerList_.end ();
	     it++)
		if (!(*it)->onKeyPress (e))
			return;
}

void
GuiObserverSubject::updateKeyRelease (const KeyEvent& e)
{
	for(std::list<GuiObserverPtr>::iterator it = observerList_.begin ();
	    it != observerList_.end ();
	    it++)
		if (!(*it)->onKeyRelease (e))
			return;
}

void
GuiObserverSubject::updateTime (const TimeEvent& e)
{
	for (std::list<GuiObserverPtr>::iterator it = observerList_.begin ();
	     it != observerList_.end ();
	     it++)
		if (!(*it)->onTime (e))
			return;
}

