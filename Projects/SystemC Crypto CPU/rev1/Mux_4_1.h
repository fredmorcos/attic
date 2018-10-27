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
	void multiplex();

	//
	//Constructor
	//
	SC_CTOR(Mux_4_1){
		SC_THREAD(multiplex);
		sensitive << k0 << k1 << i0 << i1 << i2;// << i3;
		}//end constructor
};//end module
#endif