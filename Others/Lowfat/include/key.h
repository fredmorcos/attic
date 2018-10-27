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

#ifndef _KEY_H
#define _KEY_H

class Key
{
	public:
		enum
		{
			backspace,
			tab,
			clear,
			enter,
			pause,
			escape,
			space,
			dollar,
			amp,
			quote,
			lParen,
			rParen,
			asterisk,
			plus,
			comma,
			minus,
			period,
			slash,
			digit0,
			digit1,
			digit2,
			digit3,
			digit4,
			digit5,
			digit6,
			digit7,
			digit8,
			digit9,
			colon,
			semicolon,
			less,
			equals,
			greater,
			question,
			at,
			lBracket,
			rBracket,
			caret,
			underscore,
			backquote,
			backslash,
			a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z,
			pad0,
			pad1,
			pad2,
			pad3,
			pad4,
			pad5,
			pad6,
			pad7,
			pad8,
			pad9,
			padPeriod,
			padDivide,
			padMultiply,
			padMinus,
			padPlus,
			padEnter,
			padEquals,
			up,
			del,
			down,
			right,
			left,
			insert,
			home,
			end,
			pageUp,
			pageDown,
			f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15,
			numLock,
			capsLock,
			scrollLock,
			rShift,
			lShift,
			shift,
			rCtrl,
			lCtrl,
			rAlt,
			lAlt,
			rMeta,
			lMeta,
			lSuper,
			rSuper,
			mode,
			compose,
			help,
			print,
			sysreq,
			stop,
			menu,
			power,
			euro,
			undo,
			mouse1,
			mouse2,
			mouse3,
			mouse4,
			mouse5,
			mwheelUp,
			mwheelDown
		};
};

#endif // _KEY_H
