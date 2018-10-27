#include "Mux_4_1.h"

void Mux_4_1::multiplex(){

	//sc_lv <4> kLinesTemp;
	//kLinesTemp = kLines.read();
	
	if(k1 == 0 && k0 == 0)
		output = i0;
	else if(k1 == 0 && k0 == 1)
		output = i1;
	else if(k1 == 1 && k0 == 0)
		output = i2;
	else if(k1 == 1 && k0 == 1)
		output = i3;
	}//end multiplex