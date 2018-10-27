#ifndef ADDER_32
#define ADDER_32

#include <systemc.h>

SC_MODULE(Adder_32) {
	sc_in< sc_logic > carryIn;
	sc_in< sc_lv<32> > a, b;

	sc_out< sc_lv<32> > sum;
	sc_out< sc_logic > carryOut;
	
	void add();
	
	SC_CTOR(Adder_32) {
		SC_THREAD(add);
		sensitive << a << b << carryIn;
	}
};

#endif

