/************************************************\
| soc_alu .cpp                                   |
|                                                |
| SoC Course Project                             |
| Visual Studio 2008                             |
| SystemC 2.2                                    |
| M Affan Zidan                                  |
| 23-4-2009AD                                    |  
\************************************************/

#include"soc_alu.h"

#pragma region 32-bit Adder
void Adder32Bit::Add()
{
	int i; // counter index
	sc_lv<32> aTemp, bTemp , sumTemp;
	sc_logic cTemp;

    // Read inputs	
	aTemp = a.read(); 
	bTemp = b.read();
	cTemp = carryIn.read();

	for(i=0 ; i<32; i++) // loop on all bits
	{
		sumTemp[i]= aTemp[i] ^ bTemp[i] ^ cTemp; // sum
		cTemp =  (aTemp[i] &  bTemp[i]) | (aTemp[i] & cTemp) | (bTemp[i] & cTemp); // carry
	}

	// write output
	carryOut.write(cTemp);
	sum.write(sumTemp);
	
	return;
}
#pragma endregion ///// 32-bit Adder /////

#pragma region 4:1 32-bit MUX
void MUX4to1_32Bit::Select()
{

	int i; // counter index
	sc_lv<32> temp;

	for(i=0 ; i<32; i++) // loop on all bits
	{
		temp[i] =	(~s0.read() & ~s1.read() & in0.read()[i]) |
					( s0.read() & ~s1.read() & in1.read()[i]) |
					(~s0.read() &  s1.read() & in2.read()[i]) |
					( s0.read() &  s1.read() & in3.read()[i]);
	}
	
	out.write(temp);
	return;
}
#pragma endregion ///// 4:2 32-bit MUX /////

#pragma region 32-bit And
void And32::and()
{
	out = a.read() & b.read();
	return;
}
#pragma endregion ///// 32-bit And /////

#pragma region 32-bit Or
void Or32::or()
{
	out = a.read() | b.read();
	return;
}
#pragma endregion ///// 32-bit Or /////

#pragma region 32-bit Xor
void Xor32::xor()
{
	out = a.read() ^ b.read();
	return;
}
#pragma endregion ///// 32-bit Xor /////

#pragma region 32-bit Not
void Not32::not()
{
	out = ~a.read();
	return;
}
#pragma endregion ///// 32-bit Not /////

#pragma region 32-bit Shifter
void Shifter32::Shift()
{
	sc_lv<32> inTemp, outTemp;
	sc_logic leftRightTemp;

	// Read inputs
	leftRightTemp = leftRight.read();
	inTemp = in.read();

	for(int i = 1; i<=30; i++)
	{
		outTemp[i] = inTemp[i-1] & ~leftRightTemp | // Left
					 inTemp[i+1] &  leftRightTemp;  // Right
	}
	
	outTemp[0]  = rightSerialIn.read() & ~leftRightTemp | // Left
				  inTemp[1]            &  leftRightTemp;   // Right

	outTemp[31] = inTemp[1]            & ~leftRightTemp | // Left
				  leftSerialIn.read()  &  leftRightTemp;   // Right

	// Wight Output
	out.write(outTemp);

	// Output Bit 
	outBit.write( inTemp[31] & ~leftRightTemp |  // Left
				  inTemp[0]  &  leftRightTemp);  // Right

}
#pragma endregion ///// 32-bit Shifter /////

#pragma region 32-bit Arith Unit
void Arith32::B()
{
	int i; // counter index

	sc_lv<32> bTemp = b.read();
    
	for(i=0 ; i<32; i++) // loop on all bits
	{
		bTemp[i] = (bTemp[i] & ~k0.read() ) | (~bTemp[i] & k1.read() );
	}

	// write output
	bIn.write(bTemp);
	
	return;
}
#pragma endregion ///// 32-bit Arith Unit /////

#pragma region 32-bit Shift Unit
void Shift32::SInCont()
{
	rightSerialIn = SC_LOGIC_0;
	leftSerialIn = SC_LOGIC_0 & k1.read() | in.read()[31] & ~k1.read();
	leftRight = k1.read() | k0.read();
}
#pragma endregion ///// Shift Unit /////

#pragma region 32-bit ALU
void ALU32::Flags()
{
	sc_lv<32> outTemp = out.read();

	// Sign Flag
	signFlag.write(outTemp[31] & ~k3.read() & ~k2.read() ); // Arith

	// Zero Flag
	zeroFlag.write(outTemp.nor_reduce() );

	// parity Flag
	parityFlag.write(outTemp.xor_reduce() );

	// Carry Flag
	carryFlag.write( (~k3.read() & ~k2.read() & arithCarry) |  // Arith
					 ( k3.read() & ~k2.read() & shiftCarry) ); // Shift

}
#pragma endregion ///// 32-bit ALU /////