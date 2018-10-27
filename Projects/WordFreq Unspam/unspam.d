/*	
 *	This file is part of unspam.
 *
 *	Copyright 2009	Frederic Morcos <fred.morcos@gmail.com>
 *
 *	unspam is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	unspam is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with unspam.  If not, see <http://www.gnu.org/licenses/>.
 */

module unspam;

alias char[] string;

private import
	tango.text.Util;

string[]	words;
int[]		freqs,
			spamities;

void addSpamMessage (string msg) {
	auto words = delimit(msg, " \n");
	
	foreach (w; words)
		if (w.length > 3)
			addSpamWord(w);

	spamities ~= getMessageSpamity (msg);
}

void addSpamWord (string word) {
	foreach (i, w; words)
		if (w == word) {
			++freqs[i];
			return;
		}

	words ~= word;
	freqs ~= 1;
}

bool isMessageSpam (string msg) {
	if (getMessageSpamity (msg) > getAverageSpamity)
		return true;
	return false;
}

int getMessageSpamity (string msg) {
	int spamity;
	auto words = delimit(msg, " \n");

	foreach (w; words)
		if (w.length > 3)
			spamity += getSpamWordFrequency (w);

	return spamity;
}

int getSpamWordFrequency (string word) {
	foreach (i, w; words)
		if (w == word)
			return freqs[i];
	return 0;
}

int getAverageSpamity () {
	int tmp;

	foreach (s; spamities)
		tmp += s;

	return cast(int) (tmp / spamities.length);
}

