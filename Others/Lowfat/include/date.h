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

#ifndef _DATE_H
#define _DATE_H

#include <iostream>
#include <iomanip>

class Date
{
	public:
		Date() :
			year_ (1900),
			month_ (01),
			day_ (01),
			hours_ (0),
			minutes_ (0),
			seconds_ (0)
		{
		}

		Date (int year,
		      int month,
		      int day,
		      int hours = 0,
		      int minutes = 0,
		      int seconds = 0) :
			year_ (year),
			month_ (month),
			day_ (day),
			hours_ (hours),
			minutes_ (minutes),
			seconds_ (seconds)
		{
		}

	~Date ();

	friend std::ostream& operator<<(std::ostream& s, const Date& d);

	bool
	operator<(const Date& d)
	{
		if (year_ != d.year_)
			return year_ < d.year_;
		else if (month_ != d.month_)
			return month_ < d.month_;
		else if (day_ != d.day_)
			return day_ < d.day_;
		else if (hours_ != d.hours_)
			return hours_ < d.hours_;
		else if (minutes_ != d.minutes_)
			return minutes_ < d.minutes_;
		else if (seconds_ != d.seconds_)
			return seconds_ < d.seconds_;

		return false;
	}

	private:
		int year_;
		int month_;
		int day_;
		int hours_;
		int minutes_;
		int seconds_;

		static char* monthToString (int month)
		{
			switch (month)
			{
				case 1: return "jan";
				case 2: return "feb";
				case 3: return "mar";
				case 4: return "apr";
				case 5: return "may";
				case 6: return "jun";
				case 7: return "jul";
				case 8: return "aug";
				case 9: return "sep";
				case 10: return "oct";
				case 11: return "nov";
				case 12: return "dec";
				default: return "xxx";
			};
		};
};

#endif // _DATE_H
