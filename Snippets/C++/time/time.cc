#include <iostream>
using std::cout;

#include <iomanip>
using std::setfill;
using std::setw;

#include "time.h"

time::time()
{
	hour = minute = second = 0;
}

void time::set_time(	unsigned short int h,
			unsigned short int m,
			unsigned short int s)
{
	hour = (h < 24) ? h : 0;
	minute = (m < 60) ? m : 0;
	second = (s < 60) ? s : 0;
}

void time::print_univ()
{
	cout << setfill('0') 
			<< setw(2) << hour << ":"
			<< setw(2) << minute << ":"
			<< setw(2) << second;
}

void time::print_stan()
{
	cout << ((hour == 0 || hour == 12) ? 12 : hour % 12) << ":"
		<< setfill('0') << setw(2) << minute << ":" << setw(2)
		<< second << (hour < 12 ? " AM" : " PM");
}
