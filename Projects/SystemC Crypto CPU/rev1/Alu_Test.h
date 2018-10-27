#ifndef ALU_32_GEN
#define ALU_32_GEN

#include <systemc.h>

SC_MODULE (Alu_Gen) {
	/* clock */
	sc_in<bool> clk;

	/* outputs */
	sc_out < sc_lv<32> > a, b;
	sc_out <sc_logic> k0, k1, k2, k3;

	/* methods */
	void gen();

	/* constructor */
	SC_CTOR (Alu_Gen) {
		SC_THREAD (gen);
		sensitive << clk.pos();
	}
};

SC_MODULE (Alu_Mon) {
	/* inputs */
	sc_in <bool> clk;
	sc_in <sc_logic> k0, k1, k2, k3, zero, overflow;
	sc_in < sc_lv<32> > a, b, result;
	
	/* methods */
	void check();
	
	/* constructor */
	SC_CTOR(Alu_Mon) {
		SC_THREAD(check);
		sensitive << clk.pos();
	}
};

#endif

