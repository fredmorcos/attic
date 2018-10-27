/************************************************\
| soc_alu .h                                     |
|                                                |
| SoC Course Project                             |
| Visual Studio 2008                             |
| SystemC 2.2                                    |
| M Affan Zidan                                  |
| 23-4-2009AD                                    |  
\************************************************/

#include<systemc.h>

#pragma region 32-bit Adder
SC_MODULE(Adder32Bit)
{
	// Ports
	sc_in< sc_logic > carryIn;
	sc_in< sc_lv<32> > a, b;
	sc_out< sc_lv<32> >	sum;
	sc_out< sc_logic > carryOut;

	// Processes
	void Add();

	SC_CTOR(Adder32Bit)
	{
		SC_METHOD(Add);
		sensitive<< a << b << carryIn;		
	}
};
#pragma endregion ///// 32-bit Adder /////

#pragma region 4:1 32-bit MUX
SC_MODULE(MUX4to1_32Bit)
{
	// Ports
	sc_in< sc_logic > s0, s1;
	sc_in< sc_lv<32> > in0, in1, in2, in3;
	sc_out< sc_lv<32> >	out;

	// Processes
	void Select();

	SC_CTOR(MUX4to1_32Bit)
	{
		SC_METHOD(Select);
		sensitive<< in0 << in1 << in2 << in3 << s0 << s1;		
	}
};
#pragma endregion ///// 4:2 32-bit MUX /////

#pragma region 32-bit And

SC_MODULE(And32)
{
	// Ports
	sc_in< sc_lv<32> > a, b;
	sc_out< sc_lv<32> > out;

	// Processes
	void and();

	SC_CTOR(And32)
	{
		SC_METHOD(and);
		sensitive<< a << b;
	}
};

#pragma endregion //// 32-bit And ////

#pragma region 32-bit Or

SC_MODULE(Or32)
{
	// Ports
	sc_in< sc_lv<32> > a, b;
	sc_out< sc_lv<32> > out;

	// Processes
	void or();

	SC_CTOR(Or32)
	{
		SC_METHOD(or);
		sensitive<< a << b;
	}
};

#pragma endregion //// 32-bit Or ////

#pragma region 32-bit Xor

SC_MODULE(Xor32)
{
	// Ports
	sc_in< sc_lv<32> > a, b;
	sc_out< sc_lv<32> > out;

	// Processes
	void xor();

	SC_CTOR(Xor32)
	{
		SC_METHOD(xor);
		sensitive<< a << b;
	}
};

#pragma endregion //// 32-bit Xor ////

#pragma region 32-bit Not

SC_MODULE(Not32)
{
	// Ports
	sc_in< sc_lv<32> > a;
	sc_out< sc_lv<32> > out;

	// Processes
	void not();

	SC_CTOR(Not32)
	{
		SC_METHOD(not);
		sensitive<< a;
	}
};

#pragma endregion //// 32-bit Not ////

#pragma region 32-bit Shifter

SC_MODULE(Shifter32)
{
	// Ports
	sc_in< sc_lv<32> > in;  // inputs
	sc_in< sc_logic > leftRight, leftSerialIn, rightSerialIn; // Left_right = 0 for left, 1 for right 
	sc_out< sc_lv<32> > out;  // output
	sc_out< sc_logic > outBit;

	// Processes
	void Shift();
	
	// Constructor
	SC_CTOR(Shifter32)
	{
		SC_METHOD(Shift);
		sensitive << in << leftRight << leftSerialIn << rightSerialIn;
		
	}
};

#pragma endregion //// 32-bit Shifter ///

#pragma region 32-bit Arith Unit
/*
	+---+---+-----------+
	| k1| k0| Operation |
	+---+---+-----------+
	| 0 | 0 |    Add    |
	+---+---+-----------+
	| 0 | 1 |    Inc    |
	+---+---+-----------+
	| 1 | 0 |    Dec    |
	+---+---+-----------+
	| 1 | 1 |    Sub    |
	+---+---+-----------+

Adder inputs:
Xi = Ai
Yi = Bi.K0' + Bi'.K1
Cin = K0
*/

SC_MODULE(Arith32)
{
	// Ports
	sc_in< sc_lv<32> > a, b;  // inputs
	sc_in< sc_logic > k0, k1; // selection lines
	sc_out< sc_lv<32> > out;  // output
	sc_out< sc_logic > carryOut;  // carry out

	// Signals
	sc_signal< sc_lv<32> > bIn;

	// Sub-Modules
	Adder32Bit adder;

	// Processes
	void B();

	// Constructor
	SC_CTOR(Arith32):
	adder("32-bit_Adder")
	{
		// B
		SC_METHOD(B);
		sensitive<< b << k0 << k1;

		// Adder binding
		adder.a(a);
		adder.b(bIn);
		adder.carryIn(k0);
		adder.sum(out);
		adder.carryOut(carryOut);
	}
};

#pragma endregion //// 32-bit Arith Unit ////

#pragma region 32-bit Logic Unit
/*
	+---+---+-----------+
	| k1| k0| Operation |
	+---+---+-----------+
	| 0 | 0 |    And    |
	+---+---+-----------+
	| 0 | 1 |    Or     |
	+---+---+-----------+
	| 1 | 0 |    Xor    |
	+---+---+-----------+
	| 1 | 1 |    Not    |
	+---+---+-----------+
*/

SC_MODULE(Logic32)
{
	// Ports
	sc_in< sc_lv<32> > a, b;  // inputs
	sc_in< sc_logic > k0, k1; // selection lines
	sc_out< sc_lv<32> > out;  // output

	// Sub-Modules
	MUX4to1_32Bit mux;
	And32 and;
	Or32 or;
	Xor32 xor;
	Not32 not;

	// Signals
	sc_signal< sc_lv<32> > andOutput, orOutput, xorOutput, notOutput;
	
	// Constructor
	SC_CTOR(Logic32):
	mux("Logic_MUX"), and("32-bit_And"), or("32-bit_Or"), 
		xor("32-bit_Xor"), not("32-bit_Not")
	{
		//----- Binding -----//
		// And
		and.a(a);
		and.b(b);
		and.out(andOutput);

		// Or
		or.a(a);
		or.b(b);
		or.out(orOutput);

		// Xor
		xor.a(a);
		xor.b(b);
		xor.out(xorOutput);
		
		// Not
		not.a(a);
		not.out(notOutput);

		// MUX
		mux.in0(andOutput);
		mux.in1(orOutput);
		mux.in2(xorOutput);
		mux.in3(notOutput);
		mux.s0(k0);
		mux.s1(k1);
		mux.out(out);
		
	}
};

#pragma endregion //// 32-bit Logic Unit ///

#pragma region 32-bit Shift Unit
/*
	+---+---+-----------+
	| k1| k0| Operation |
	+---+---+-----------+
	| 0 | 0 |    SHL    |
	+---+---+-----------+
	| 0 | 1 |    SAR    |
	+---+---+-----------+
	| 1 | X |    SHR    |
	+---+---+-----------+

*/
SC_MODULE(Shift32)
{
	// Ports
	sc_in< sc_lv<32> > in;  // inputs
	sc_in< sc_logic > k0, k1; 
	sc_out< sc_lv<32> > out;  // output
	sc_out< sc_logic > carryFlag;

	// Sub-Modules
	Shifter32 shifter;

	// Signals
	sc_signal< sc_logic > leftSerialIn, rightSerialIn, leftRight;

	// Processes
	void SInCont();
	
	// Constructor
	SC_CTOR(Shift32):
	shifter("32-bit_Shifter")
	{
		SC_METHOD(SInCont);
		sensitive << in << k0 << k1;

		//---- Binding ----//
		shifter.in(in);
		shifter.out(out);
		shifter.leftRight(leftRight);
		shifter.leftSerialIn(leftSerialIn);
		shifter.rightSerialIn(rightSerialIn);
		shifter.outBit(carryFlag);	
	}
};

#pragma endregion //// 32-bit Shift Unit /////

#pragma region 32-bit ALU

/*
	+---+---+
	| k3| k2|
	+---+---+-----------+
	| 0 | 0 |   Arith   |
	+---+---+-----------+
	| 0 | 1 |   Logic   |
	+---+---+-----------+
	| 1 | 0 |   Shift   |
	+---+---+-----------+
	| 1 | 1 |  not used |
	+---+---+-----------+
*/


SC_MODULE(ALU32)
{
	// Ports
	sc_in< sc_lv<32> > a, b;  // Inputs
	sc_in<sc_logic> k0, k1, k2, k3;  // Controls
	sc_out< sc_lv<32> > out;  // Output
	sc_out< sc_logic> carryFlag, zeroFlag, parityFlag, signFlag;  // Flags

	// Sub-Modules
	Arith32 arith;
	Logic32 logic;
	Shift32 shift;
	MUX4to1_32Bit mux;

	// Processes
	void Flags();

	// Signals
	sc_signal< sc_lv<32> > arithOutput, logicOutput, ShiftOutput;
	sc_signal< sc_logic > arithCarry, shiftCarry;

	// Constructor
	SC_CTOR(ALU32):
	arith("32-bit_Arith_Unit"), logic("32-bit_Logic_Unit"), 
	shift("32-bit_Shift_Unit"), mux("ALU_MUX")
	{
		// Flags
		SC_METHOD(Flags);
		sensitive << out << k2 << k3 << arithCarry << shiftCarry;

		//------ Binding ------//

		// Arith Unit
		arith.a(a);
		arith.b(b);
		arith.k0(k0);
		arith.k1(k1);
		arith.out(arithOutput);
		arith.carryOut(arithCarry);

		// Logic Unit
		logic.a(a);
		logic.b(b);
		logic.k0(k0);
		logic.k1(k1);
		logic.out(logicOutput);

		// Shift Unit
		shift.in(a);
		shift.k0(k0);
		shift.k1(k1);
		shift.out(ShiftOutput);
		shift.carryFlag(shiftCarry);

		// MUX
		mux.in0(arithOutput);
		mux.in1(logicOutput);
		mux.in2(ShiftOutput);
		mux.in3(ShiftOutput);
		mux.s0(k2);
		mux.s1(k3);
		mux.out(out);
	}
};

#pragma endregion //// 32-bit ALU ////