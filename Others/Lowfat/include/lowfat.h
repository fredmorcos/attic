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

#ifndef _LOWFAT_H
#define _LOWFAT_H

#include "include/background.h"
#include "include/selection.h"
#include "include/photo_observer.h"
#include "include/application.h"

typedef std::list<std::string> StringList;

class Lowfat : public Application
{
	public:
		Lowfat (std::string title = "lowfat",
			float width = 800.0f,
			float height = 600.0f,
			bool fullscreenFlag = true,
			int argc = 0,
			char** argv = NULL);
		virtual ~Lowfat ();
		virtual void paint (LfDisplay& display);
		virtual void onMouseButton (int x,
					    int y,
					    int button,
					    bool isDown);
		virtual void onMouseMove (int x,
					  int y);
		virtual void onKey (int key,
				    bool isDown);
		virtual void advance (unsigned int);
		virtual void DrawLoadInfo (std::string filename);
		void displayLoadImages ();
		BackgroundPtr getBackground ();
		SelectionPtr getSelection ();
		void enableSelection (bool enable);
		bool hasSelection ();
		Vec2f getTrashCanLocation () const;
		void thread_function (StringList* filenameList);
		StringList dialog ();

	private:
		BackgroundPtr	background_;
		SelectionPtr	selection_;
		int		mouseX_;
		int		mouseY_;
		bool		enableSelection_;
		bool		showInfo_;
		std::string	info_;
		float		infoAlpha_;
		unsigned int	lastSelect_;
		unsigned int	lastTime_;
		int		argc_;
		char**		argv_;
};

#endif // _LOWFAT_H
