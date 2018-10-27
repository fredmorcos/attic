#ifndef MUX_4_1
#define MUX_4_1

#include <systemc.h>

SC_MODULE(Mux_4_1){
	//
	//Ports
	//
	sc_in < sc_logic > k0, k1;
	sc_in < sc_lv <32> > i0, i1, i2, i3;
	sc_out < sc_lv <32> > output;

	//
	//The only process
	//
	void multiplex(){

		//sc_lv <4> kLinesTemp;
		//kLinesTemp = kLines.read();
		
		if(k1 == 0 && k0 == 0)
			output.write(i0);
		else if(k1 == 0 && k0 == 1)
			output.write(i1);
		else if(k1 == 1 && k0 == 0)
			output.write(i2);
		else if(k1 == 1 && k0 == 1)
			output.write(i3);
		else
			output.write(i0);
		}//end multiplex

	//
	//Constructor
	//
	SC_CTOR(Mux_4_1){
		SC_METHOD(multiplex);
		sensitive << k0 << k1 << i0 << i1 << i2 << i3;
		}//end constructor
};//end module
#endif