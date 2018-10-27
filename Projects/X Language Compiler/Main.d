/*	
 *	This file is part of xc.
 *
 *	Copyright 2009	Frederic Morcos <fred.morcos@gmail.com>
 *
 *	xc is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	xc is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with xc.  If not, see <http://www.gnu.org/licenses/>.
 */

module Main;

private import
	Parser,
	Extra,
	tango.io.Stdout;

int main (string[] args) {
	if (args.length <= 1) {
		Stdout("Usage: ./xc <input-filename>").newline;
		return 0;
	}
	
	Parser p = new Parser (args[1]);
	p.run;
	
	return 0;
}

