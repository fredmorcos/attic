#include <systemc.h>
#include "ALU.h"
#include "Alu_Test.h"
//#include "OR_32_Test.h"
//#include "OR_32.h"

int sc_main(int argc, char* argv[])
	{

	//ALU
	Alu alu("ALU");
	Alu_Gen alu_generator("Generator");
	Alu_Mon alu_monitor("Monitor");
	sc_clock clk ("Clock", 10, SC_NS, 0.5, 50, SC_NS, true);

	//Signal
	sc_signal <sc_lv <32> > a, b, result;
	sc_signal <sc_logic> overflow, zeroBit, k0, k1, k2, k3;

	//OR_gate block
	/*Or_32 orGate("32 bit OR gate");
	Generator generator("Generator");
	Monitor monitor("Monitor");
	sc_clock clk ("Clock", 10, SC_NS, 0.5, 50, SC_NS, true);*/

	//
	//Bind
	//
	/*orGate.a(a);
	orGate.b(b);
	orGate.result(result);*/

	alu.a(a);
	alu.b(b);
	alu.result(result);
	alu.k0(k0);
	alu.k1(k1);
	alu.k2(k2);
	alu.k3(k3);
	alu.zeroFlag(zeroBit);
	alu.overflowFlag(overflow);

	//Generator
	alu_generator.a(a);
	alu_generator.b(b);
	alu_generator.clk(clk);
	
	alu_generator.k0(k0);
	alu_generator.k1(k1);
	alu_generator.k2(k2);
	alu_generator.k3(k3);

	//Monitor
	alu_monitor.a(a);
	alu_monitor.b(b);
	alu_monitor.result(result);

	alu_monitor.k0(k0);
	alu_monitor.k1(k1);
	alu_monitor.k2(k2);
	alu_monitor.k3(k3);

	alu_monitor.zero(zeroBit);
	alu_monitor.overflow(overflow);
	alu_monitor.clk(clk);
	
	//Start simulation
	sc_start();

	return 0;
	}//end main
