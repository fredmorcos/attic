#include <systemc.h>
#include "Adder_32.h"
#include "And_32.h"
#include "OR_32.h"
#include "Or_1.h"
#include "Inverter_32.h"
#include "Mux_4_1.h"
#include "Nor_32.h"

SC_MODULE(Alu) {

	// inputs
	sc_in < sc_lv<32> > a, b;
	sc_in < sc_logic > k0, k1, k2, k3;	
	
	// outputs
	sc_out < sc_lv<32> > result;
	sc_out <sc_logic> zeroFlag, overflowFlag;

	//
	//Create sub-modules
	//
	And_32 andGate;
	Or_32 orGate;
	Adder_32 adder_subtractor;
	Inverter_32 norInverter;
	Inverter_32 subtractorInverter;
	Or_1 or_nor_selector_OR_gate;
	Mux_4_1 multiplexer;
	Nor_32 zero_flag_nor;

	//
	//Signals (for internal propagation)
	//
	sc_signal < sc_lv <32> > adder_subtractor_output, adder_subtractor_second_operand;
	sc_signal < sc_lv <32> > or_output, or_nor_output, andOutput, norInverterInput, andInput;
	sc_signal <sc_logic> or_nor_selector_output;
	sc_signal <sc_logic> overflowSignal, zeroFlagSignal;


	//void exec();

	
	SC_CTOR(Alu): andGate("32_bit_AND_gate"),
		orGate("32_bit_OR_gate"),
		adder_subtractor("32_bit_adder_subtractor"),
		norInverter("NOR_inverter"),
		subtractorInverter("Subtractor_inverter"),
		or_nor_selector_OR_gate("Or_gate"),
		multiplexer("4_1_Multiplexer"),
		zero_flag_nor("Zero_flag_NOR"){
		

		//
		//Connect sub-modules
		//
	
		//And gate
		andGate.a( a );
		andGate.b( b );
		andGate.result( andOutput );

		//Or gate
		orGate.a( a );
		orGate.b( b );
		orGate.result( or_output );
		//or_output.write(temp.read());
		//sc_lv<32> norinverterinput = or_output.read();
		
		//Nor inverter
		norInverter.enable( k3 );
		norInverter.input( or_output );
		norInverter.out( or_nor_output );

		//Adder/subtractor inverter
		subtractorInverter.enable( k2 );
		subtractorInverter.input( b );
		subtractorInverter.out( adder_subtractor_second_operand );

		//Adder / subtractor
		adder_subtractor.a( a );
		adder_subtractor.b( adder_subtractor_second_operand );
		adder_subtractor.carryIn( k2 );
		adder_subtractor.sum( adder_subtractor_output );
		adder_subtractor.carryOut( overflowSignal );
		overflowFlag = overflowSignal;				//Overflow flag

		//Or gate for MUX select
		or_nor_selector_OR_gate.a( k3 );
		or_nor_selector_OR_gate.b( k0 );
		or_nor_selector_OR_gate.result ( or_nor_selector_output );
		
		//MUX
		multiplexer.i0( andOutput );
		multiplexer.i1( or_nor_output );
		multiplexer.i2( adder_subtractor_output );
		multiplexer.i3 ( adder_subtractor_output );	//Dummy
		multiplexer.k0( k0 );
		multiplexer.k1( k1 );
		multiplexer.output( result );
		
		//Zero flag
		zero_flag_nor.a( result );
		zero_flag_nor.result( zeroFlagSignal );
		zeroFlag = zeroFlagSignal;

		//
		//The only ALU process
		//
		//SC_THREAD(exec);
		//sensitive << a << b << k0 << k1 << k2 << k3;
	}
};
