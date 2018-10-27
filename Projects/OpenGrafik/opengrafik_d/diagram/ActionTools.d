/*
 *	This file is part of OpenGrafik.
 *
 *	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>
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

module core.ActionTools;

enum ActionTool {
	ACTION_TOOLS_SELECT,
	ACTION_TOOLS_MOVE,
	ACTION_TOOLS_ROTATE,
	ACTION_TOOLS_SCALE
}	

class ActionTools {
protected:
	static ActionTool currentTool = ACTION_TOOLS_SELECT;
	
public:
	static ActionTool getCurrentTool () {
		return currentTool;
	}
	
	static void setCurrentTool (ActionTool at) {
		currentTool = at;
	}
}

