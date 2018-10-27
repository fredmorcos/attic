#ifndef ADDER_32_GEN
#define ADDER_32_GEN

#include <systemc.h>

SC_MODULE (Adder_32_Gen) {
	sc_in<bool> clk;

	sc_out< sc_lv<32> > a, b;
	sc_out< sc_logic > carryIn;

	void gen();

	SC_CTOR (Adder_32_Gen) {
		SC_THREAD (gen);
		sensitive << clk.pos();
	}
};

SC_MODULE (Adder_32_Mon) {
	sc_in<bool> clk;
	sc_in< sc_logic > carryIn, carryOut;
	sc_in< sc_lv<32> > a, b, sum;
	
	void check();
	
	SC_CTOR(Adder_32_Mon) {
		SC_THREAD(check);
		sensitive << clk.pos();
	}
};

#endif

