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

module core.DObject;

alias char[] string;

struct CallbackData {
	string	signal;
	Object	object;
	Object	info;
}

struct Callback {
	string signal;
	void delegate (CallbackData*) callback;
}

class DObject {
protected:
	Callback*[]	callbackList;
	string[]	signalList;
	
	bool signalRegistered (string signal) {
		foreach (string s; signalList)
			if (s == signal)
				return true;
		return false;
	}
	
	void registerSignal (string signal) {
		foreach (string s; signalList)
			if (s == signal)
				return;
		
		signalList ~= signal;
	}
	
	void emitSignal (string signalName, Object info = null) {
		CallbackData* data = null;
		
		foreach (Callback* cb; callbackList) {
			if (cb.signal == signalName) {
				if (data == null) {
					data = new CallbackData;
					data.signal = signalName;
					data.object = this;
					data.info = info;
				}
				cb.callback(data);
			}
		}
	}
	
public:
	void addCallback (string signalName, void delegate (CallbackData*) cb) {
		if (signalRegistered(signalName) == false)
			throw new Exception ("Signal not registered.");
		
		Callback *tmp = new Callback;
		tmp.signal = signalName;
		tmp.callback = cb;
		
		callbackList ~= tmp;
	}
}

