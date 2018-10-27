#ifndef INVERTER_32
#define INVERTER_32

#include <systemc.h>

SC_MODULE(Inverter_32) {

	//
	//Ports
	//
	sc_in < sc_lv <32> > input;
	sc_in < sc_logic> enable;
	sc_out < sc_lv<32> > out;

	void invert();
	
	SC_CTOR(Inverter_32) {
		SC_THREAD(invert);
		sensitive << input << enable;
	}
};

#endif

