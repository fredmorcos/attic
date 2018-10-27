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

#ifndef _APPLICATION_H
#define _APPLICATION_H

#include "include/lfdisplay.h"
#include "include/shared_ptr.h"
#include "include/lfwindow.h"
#include "include/gui_observer.h"

class Application : public LfWindow, public GuiObserverSubject
{
	public:
		Application (std::string title = "application",
			     float width = 800.0f,
			     float height = 600.0f,
			     bool fullscreenFlag = true);

		virtual ~Application ();

		void run ();
		void quit ();

		virtual void onMouseButton (int x,
					    int y,
					    int button,
					    bool isDown)
		{
		};

		virtual void onMouseMove (int x, int y)
		{
		};

		virtual void onKey (int key, bool isDown)
		{
		};

		virtual Vec2f getSize () const;
		virtual float getWidth () const;
		virtual float getHeight () const;
		virtual void DrawLoadInfo (std::string filename)
		{
		};

		static Vec2f getAverageMouseMoveVelocity ();
		LfDisplayPtr display_;

	private:
		void SaveImagePositions ();
		void LoadOldImages ();
		bool isActive_;
};

#endif // _APPLICATION_H
