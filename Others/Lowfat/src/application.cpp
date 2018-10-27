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

#include <list>
#include <ostream>
#include <fstream>
#include <sstream>
#include <GL/gl.h>
#include <GL/glu.h>
#include <stdexcept>

#include "SDL.h"
#include "include/application.h"
#include "include/key.h"
#include "include/gui_observer.h"

using std::list;

class MouseMoveBuffer
{
	public:
		MouseMoveBuffer (float total) :
			total_ (total)
		{
			add (0, 0, 1.0f);
		}

		void
		add (int dx,
		     int dy,
		     float delta)
		{
			if (delta < 0.00001f)
			{
				(*moves_.begin ()).dx += dx;
				(*moves_.begin ()).dy += dy;
			}
			else
			{
				if (((*moves_.begin ()).dx == 0) &&
				    ((*moves_.begin ()).dy == 0) &&
				    (dx == 0) &&
				    (dy == 0))
				{
					(*moves_.begin ()).delta += delta;
				}
				else
				{
					MouseMove mouseMove;
					mouseMove.dx = dx;
					mouseMove.dy = dy;
					mouseMove.delta = delta;
					moves_.push_front (mouseMove);
				}
			}
		}

		Vec2f
		getVelocity ()
		{
			Vec2f vel;
			float time (0);

			for (list<MouseMove>::iterator it (moves_.begin ());
			     it != moves_.end();
			     ++it)
			{
				MouseMove move (*it);
				if ((time + move.delta) > total_)
				{
					const float diff (total_ - (time + move.delta));
					vel += Vec2f (move.dx, move.dy) * diff;
					time = total_;
					++it;
					moves_.erase (it, moves_.end ());
					break;
				}

				time += move.delta;
				vel += Vec2f (move.dx, move.dy);
			}

			return vel / time;
		}

	private:
		float total_;

		struct MouseMove
		{
			int dx;
			int dy;
			float delta;
		};

		list<MouseMove> moves_;
};

static MouseMoveBuffer mouseMoveBuffer (0.05f);

static int mapKey(int sdlKey)
{
	switch (sdlKey)
	{
		case SDLK_BACKSPACE: return Key::backspace;
		case SDLK_TAB: return Key::tab;
		case SDLK_CLEAR: return Key::clear;
		case SDLK_RETURN: return Key::enter;
		case SDLK_PAUSE: return Key::pause;
		case SDLK_ESCAPE: return Key::escape;
		case SDLK_SPACE: return Key::space;
		case SDLK_DOLLAR: return Key::dollar;
		case SDLK_AMPERSAND: return Key::amp;
		case SDLK_QUOTE: return Key::quote;
		case SDLK_LEFTPAREN: return Key::lParen;
		case SDLK_RIGHTPAREN: return Key::rParen;
		case SDLK_ASTERISK: return Key::asterisk;
		case SDLK_PLUS: return Key::plus;
		case SDLK_COMMA: return Key::comma;
		case SDLK_MINUS: return Key::minus;
		case SDLK_PERIOD: return Key::period;
		case SDLK_SLASH: return Key::slash;
		case SDLK_0: return Key::digit0;
		case SDLK_1: return Key::digit1;
		case SDLK_2: return Key::digit2;
		case SDLK_3: return Key::digit3;
		case SDLK_4: return Key::digit4;
		case SDLK_5: return Key::digit5;
		case SDLK_6: return Key::digit6;
		case SDLK_7: return Key::digit7;
		case SDLK_8: return Key::digit8;
		case SDLK_9: return Key::digit9;
		case SDLK_COLON: return Key::colon;
		case SDLK_SEMICOLON: return Key::semicolon;
		case SDLK_LESS: return Key::less;
		case SDLK_EQUALS: return Key::equals;
		case SDLK_GREATER: return Key::greater;
		case SDLK_QUESTION: return Key::question;
		case SDLK_AT: return Key::at;
		case SDLK_LEFTBRACKET: return Key::lBracket;
		case SDLK_BACKSLASH: return Key::backslash;
		case SDLK_RIGHTBRACKET: return Key::rBracket;
		case SDLK_CARET: return Key::caret;
		case SDLK_UNDERSCORE: return Key::underscore;
		case SDLK_BACKQUOTE: return Key::backquote;
		case SDLK_a: return Key::a;
		case SDLK_b: return Key::b;
		case SDLK_c: return Key::c;
		case SDLK_d: return Key::d;
		case SDLK_e: return Key::e;
		case SDLK_f: return Key::f;
		case SDLK_g: return Key::g;
		case SDLK_h: return Key::h;
		case SDLK_i: return Key::i;
		case SDLK_j: return Key::j;
		case SDLK_k: return Key::k;
		case SDLK_l: return Key::l;
		case SDLK_m: return Key::m;
		case SDLK_n: return Key::n;
		case SDLK_o: return Key::o;
		case SDLK_p: return Key::p;
		case SDLK_q: return Key::q;
		case SDLK_r: return Key::r;
		case SDLK_s: return Key::s;
		case SDLK_t: return Key::t;
		case SDLK_u: return Key::u;
		case SDLK_v: return Key::v;
		case SDLK_w: return Key::w;
		case SDLK_x: return Key::x;
		case SDLK_y: return Key::y;
		case SDLK_z: return Key::z;
		case SDLK_DELETE: return Key::del;
		case SDLK_KP0: return Key::pad0;
		case SDLK_KP1: return Key::pad1;
		case SDLK_KP2: return Key::pad2;
		case SDLK_KP3: return Key::pad3;
		case SDLK_KP4: return Key::pad4;
		case SDLK_KP5: return Key::pad5;
		case SDLK_KP6: return Key::pad6;
		case SDLK_KP7: return Key::pad7;
		case SDLK_KP8: return Key::pad8;
		case SDLK_KP9: return Key::pad9;
		case SDLK_KP_PERIOD: return Key::padPeriod;
		case SDLK_KP_DIVIDE: return Key::padDivide;
		case SDLK_KP_MULTIPLY: return Key::padMultiply;
		case SDLK_KP_MINUS: return Key::padMinus;
		case SDLK_KP_PLUS: return Key::padPlus;
		case SDLK_KP_ENTER: return Key::padEnter;
		case SDLK_KP_EQUALS: return Key::padEquals;
		case SDLK_UP: return Key::up;
		case SDLK_DOWN: return Key::down;
		case SDLK_RIGHT: return Key::right;
		case SDLK_LEFT: return Key::left;
		case SDLK_INSERT: return Key::insert;
		case SDLK_HOME: return Key::home;
		case SDLK_END: return Key::end;
		case SDLK_PAGEUP: return Key::pageUp;
		case SDLK_PAGEDOWN: return Key::pageDown;
		case SDLK_F1: return Key::f1;
		case SDLK_F2: return Key::f2;
		case SDLK_F3: return Key::f3;
		case SDLK_F4: return Key::f4;
		case SDLK_F5: return Key::f5;
		case SDLK_F6: return Key::f6;
		case SDLK_F7: return Key::f7;
		case SDLK_F8: return Key::f8;
		case SDLK_F9: return Key::f9;
		case SDLK_F10: return Key::f10;
		case SDLK_F11: return Key::f11;
		case SDLK_F12: return Key::f12;
		case SDLK_F13: return Key::f13;
		case SDLK_F14: return Key::f14;
		case SDLK_F15: return Key::f15;
		case SDLK_NUMLOCK: return Key::numLock;
		case SDLK_CAPSLOCK: return Key::capsLock;
		case SDLK_SCROLLOCK: return Key::scrollLock;
		case SDLK_RSHIFT: return Key::shift;
		case SDLK_LSHIFT: return Key::shift;
		case SDLK_RCTRL: return Key::rCtrl;
		case SDLK_LCTRL: return Key::lCtrl;
		case SDLK_RALT: return Key::rAlt;
		case SDLK_LALT: return Key::lAlt;
		case SDLK_RMETA: return Key::rMeta;
		case SDLK_LMETA: return Key::lMeta;
		case SDLK_LSUPER: return Key::lSuper;
		case SDLK_RSUPER: return Key::rSuper;
		case SDLK_MODE: return Key::mode;
		case SDLK_COMPOSE: return Key::compose;
		case SDLK_HELP: return Key::help;
		case SDLK_PRINT: return Key::print;
		case SDLK_SYSREQ: return Key::sysreq;
		case SDLK_BREAK: return Key::stop;
		case SDLK_MENU: return Key::menu;
		case SDLK_POWER: return Key::power;
		case SDLK_EURO: return Key::euro;
		case SDLK_UNDO: return Key::undo;
		default: throw std::runtime_error ("unknown key");
	};
}

Application::Application (std::string title,
			  float width,
			  float height,
			  bool fullscreenFlag ) : 
    isActive_ (true)
{
	display_ = LfDisplay::make (title, width, height, fullscreenFlag);
	SDL_EnableKeyRepeat (SDL_DEFAULT_REPEAT_DELAY,
			     SDL_DEFAULT_REPEAT_INTERVAL);
}

Application::~Application ()
{
}

std::string
GetStatePath ()
{
	std::string strPath("./");
	strPath += "state.txt";
	return strPath;
}

void
Application::SaveImagePositions ()
{
	std::ofstream stream (GetStatePath ().c_str ());

	for (ImageSeq::iterator it (images_.begin ());
	     it != images_.end();
	     ++it)
	{
		Vec2f location = (*it)->getLocation ();
		float rotation = (*it)->getRotation ();
		Vec2f size = (*it)->getSize ();
		stream << (*it)->getPath()
		       << "\n"
		       << location.x
		       << "\n"
		       << location.y
		       << "\n"
		       << rotation
		       << "\n"
		       << size.x
		       << "\n"
		       << size.y
		       << "\n";
	}

	stream.close ();
}

void
Application::LoadOldImages ()
{
	std::ifstream stream (GetStatePath ().c_str ());
	std::string strX;
	std::string strY;
	std::string strRot;
	float locX;
	float locY;
	float sizeX;
	float sizeY;
	float rotation;
	char path[512];
	char temp[512];

	while (stream)
	{
		try
		{
			stream.getline (path, 99999, '\n');
			stream.getline (temp, 99999, '\n');
			locX = atof (temp);
			stream.getline (temp, 99999, '\n');
			locY = atof (temp);
			stream.getline (temp, 99999, '\n');
			rotation = atof (temp);
			stream.getline (temp, 99999, '\n');
			sizeX = atof (temp);
			stream.getline (temp, 99999, '\n');
			sizeY = atof (temp);

			if (!stream)
				break;

			advance (SDL_GetTicks ());
			TimeEvent timeEvent;
			timeEvent.time = SDL_GetTicks ();
			updateTime (timeEvent);

			display_.get()->clear ();
			paint (*display_.get ());
			DrawLoadInfo (path);
			ImagePtr image (Image::make (path));
			image->setLocation (locX, locY);
			image->setRotation (rotation);
			image->setSize (sizeX, sizeY);
			addImage (image);
		}
		catch(...)
		{
		}
	}

	stream.close ();
}

void
Application::quit()
{
	SaveImagePositions ();
	isActive_ = false;
}

Vec2f
Application::getSize () const
{
	return Vec2f (getWidth (), getHeight ());
}

float
Application::getWidth () const
{
	return display_->getWidth ();
}

float
Application::getHeight () const
{
	return display_->getHeight ();
}

void
Application::run ()
{
	SDL_Event ev_event;

	LoadOldImages ();

	long int lastTime = 0;

	int mouseX (0);
	int mouseY (0);
	int lastX (0);
	int lastY (0);

	while (isActive_)
	{
	        while (SDL_PollEvent(&ev_event))
		{
			switch (ev_event.type)
			{
				case SDL_QUIT :
					quit ();
				break;

				case SDL_MOUSEBUTTONUP:
				case SDL_MOUSEBUTTONDOWN:
				{
					bool state = (((SDL_MouseButtonEvent*) &ev_event)->state) == SDL_PRESSED;
					int x = ((SDL_MouseButtonEvent*) &ev_event)->x;
					int y = ((SDL_MouseButtonEvent*) &ev_event)->y;
					int button = 0;

					switch (((SDL_MouseButtonEvent*) &ev_event)->button)
					{
						case SDL_BUTTON_LEFT:
							button = MouseButton::left;
						break;

						case SDL_BUTTON_MIDDLE:
							button = MouseButton::mid;
						break;

						case SDL_BUTTON_RIGHT:
							button = MouseButton::right;
						break;

						case SDL_BUTTON_WHEELUP:
							button = MouseButton::wheelUp;
						break;

						case SDL_BUTTON_WHEELDOWN:
							button = MouseButton::wheelDown;
						break;

						default:
						break;
					};

					onMouseButton (x, y, button, state);
					MouseEvent mouseEvent;
					mouseEvent.x = x;
					mouseEvent.y = y;
					mouseEvent.button = button;
					if (state)
						updateMousePress (mouseEvent);
					else
						updateMouseRelease (mouseEvent);
				}
				break;

				case SDL_MOUSEMOTION:
				{
					onMouseMove (((SDL_MouseMotionEvent*) &ev_event)->x,
						     ((SDL_MouseMotionEvent*) &ev_event)->y);
					MouseEvent mouseEvent;
					mouseX = mouseEvent.x = ((SDL_MouseMotionEvent*) &ev_event)->x;
					mouseY = mouseEvent.y = ((SDL_MouseMotionEvent*) &ev_event)->y;
					mouseEvent.button = 0;
					updateMouseMove (mouseEvent);
					break;
				}

				case SDL_KEYUP:
				{
					onKey (mapKey (ev_event.key.keysym.sym),
					       false);
					KeyEvent keyEvent;
					keyEvent.key = mapKey (
						ev_event.key.keysym.sym);
					updateKeyRelease (keyEvent);
					break;
				}

				case SDL_KEYDOWN:
				{
					onKey (mapKey (ev_event.key.keysym.sym),
					       true);
					KeyEvent keyEvent;
					keyEvent.key = mapKey (
						ev_event.key.keysym.sym);
					updateKeyPress (keyEvent);
					break;
				}

				case SDL_VIDEORESIZE:
				{
					int width = ((SDL_ResizeEvent*) &ev_event)->w;
					int height = ((SDL_ResizeEvent*) &ev_event)->h;
					display_->setSize (width, height);
					break;
				}

			}
		}

		// draw the whole screen/desktop/window
		advance (SDL_GetTicks ());
		TimeEvent timeEvent;
		timeEvent.time = SDL_GetTicks ();
		const float delta ((timeEvent.time - lastTime) / 1000.0f);
		mouseMoveBuffer.add (mouseX - lastX, mouseY - lastY, delta);
		lastX = mouseX;
		lastY = mouseY;
		lastTime = timeEvent.time;
		updateTime (timeEvent);

		paint (*display_.get ());
		display_->flip ();
	}
}

Vec2f
Application::getAverageMouseMoveVelocity ()
{
	return mouseMoveBuffer.getVelocity ();
}

