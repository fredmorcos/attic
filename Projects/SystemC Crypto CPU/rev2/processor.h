#ifndef _processor_
#define _processor_

#include <systemc.h>
#include "soc_alu.h"

SC_MODULE(Processor) { 

	sc_inout< sc_lv<8> > MDR;
	sc_out< sc_lv<16> > MAR;
	sc_out< sc_logic > memW, memR;
	sc_in<bool> clk;
	
	/* Memory */
	sc_lv<32> [100] memory;
	memory[0].write(00000100010001000100000000000000);
	memory[1].write(00001000010100000100000000000000);
	memory[2].write(00001110000010000010000000000000);
	memory[3].write(00010010000010000010000000000000);
	memory[4].write(00010110000001000001000000000000);
	memory[5].write(01001100010001000000000000000000);
	memory[6].write(01010100010001000000000000000000);
	memory[7].write(01011000010001000000000000000000);
	memory[8].write(01011101000001000000000000000000);
	memory[9].write(01100000010001000000000000000000);
	memory[10].write(11010010000000000001100100000000); //100
	memory[11].write(11000000000000011001000000000000);
	memory[12].write(11100000010000100000000000000000);
	memory[13].write(11100100010001000000000000000000);
	
	/* data - memory is 8 bits wide. */
	sc_signal< sc_lv<32> > IR, AC;
	
	/* address register and program counter */
	sc_signal< sc_lv<16> > PC;
	
	/* note: add is also subtract, branch is branch-equal and
	 * branch-not-equal
	 */
	enum states {fetch1, fetch2, fetch3, decode, load1, load2, load3, load4,
				 store1, store2, store3, store4, sub1, sub2, add1, add2, 
				 and1, and2, or1, or2, xor1, xor2, shl1, shl2, shr1, 
				 shr2, mov1, mov2, loadi, jump, beq1, beq2, bneq1, bneq2 };
	
	sc_signal< states > CS, NS;
	
	/* all operations result */
	sc_signal< sc_lv<32> > result;
	
	/* opcode */
	sc_signal< sc_lv<6> > opcode;
	
	/* ALU K selections and inputs */
	sc_signal <sc_logic> k0_signal, 
						 k1_signal, 
						 k2_signal,
						 k3_signal;
	sc_signal< sc_lv<32> > a_input, b_input, alu_output;
	sc_out< sc_logic> carryFlag, zeroFlag, parityFlag, signFlag;
	/* RF input */
	sc_signal< sc_lv<32> > opr_output;
	sc_lv<32> tmp_val;
	
	sc_lv<32> tmp1, tmp2;			 
	
	void comb();
	void seq();
	void incPC();
	
	ALU32 alu;
	/* Register file */
	RegisterFile reg_file;
	
	SC_CTOR(SimpleProcessor): alu("ALU") {
		PC.write(0xFFFF);
		CS = NS = fetch_1;
		SC_METHOD(comb);
		sensitive << CS;
		SC_METHOD(seq);
	    sensitive << clk.pos();
	    // Sub-modules bindingUDS
	    // Binding register file output with ALU
	    alu.a(a_input);
	    alu.b(b_input);reg_file.
	    alu.out(alu_output);
	    alu.k0(k0_signal);
	    alu.k1(k1_signal);
	    alu.k2(k2_signal);
	    alu.k3(k3_signal);
	    alu.carryFlag(carryFlag);
	    alu.zeroFlag(zeroFlag); 
	    alu.parityFlag(parityFlag); 
	    alu.signFlag(signFlag);
	    
	    reg_file.input(opr_output);
	}
};

void Processor::comb () {
	switch(CS) {
		// fetch
		case fetch1:
			PC.write(PC.read() + 0x1);
			MAR.write(PC.read());
			NS = fetch2;
			break;
		case fetch2:
			memR.write( SC_LOGIC_1 );
			incPC();
			NS = fetch_3;
			break;
		case fetch3:
			IR.write(MDR.read());
			NS = decode;
			break;
		// decode	
		case decode:
			sc_lv<32> irTmp = IR.read();
			if (irTmp.range(31, 26) == 0x1)
				NS = add1;
			else if (irTmp.range(31, 26) == 0x2)
				NS = sub1;
			else if (irTmp.range(31, 26) == 0x3)
				NS = and1;
			else if (irTmp.range(31, 26) == 0x4)
				NS = or1;
			else if (irTmp.range(31, 26) == 0x5)
				NS = xor1;
			else if (irTmp.range(31, 26) == 0x13)
				NS = shl1;
			else if (irTmp.range(31, 26) == 0x15)
				NS = shr1;
			else if (irTmp.range(31, 26) == 0x16)
				NS = move;
			else if (irTmp.range(31, 26) == 0x17)
				NS = load1;
			else if (irTmp.range(31, 26) == 0x18)
				NS = store1;
			else if (irTmp.range(31, 26) == 0x34)
				NS = loadi;
			else if (irTmp.range(31, 26) == 0x30)
				NS = jump;
			else if (irTmp.range(31, 26) == 0x38)
				NS = bq1;
			else if (irTmp.range(31, 26) == 0x39)
				NS = bneq1;
			break;
		/* execution */	
		case add1:
			k3_signal.write(SC_LOGIC_0);
			k2_signal.write(SC_LOGIC_0);
			k1_signal.write(SC_LOGIC_0);
			k0_signal.write(SC_LOGIC_0);
			a_input.write(reg_file.output1);
			b_input.write(reg_file.output2);
			NS = add2;
			break;
		case add2:
			opr_output.write(alu_output);
			NS = fetch1;
			break;
//		case add3:
//			break;
		case sub1:
			k3_signal.write(SC_LOGIC_0);
			k2_signal.write(SC_LOGIC_0);
			k1_signal.write(SC_LOGIC_1);
			k0_signal.write(SC_LOGIC_1);
			a_input.write(reg_file.output1);
			b_input.write(reg_file.output2);
			NS = sub2;
			break;
		case sub2:
			opr_output.write(alu_output);
			NS = fetch1;
			break;
//		case sub3:
//			break;
		case and1:
			k3_signal.write(SC_LOGIC_0);
			k2_signal.write(SC_LOGIC_1);
			k1_signal.write(SC_LOGIC_0);
			k0_signal.write(SC_LOGIC_0);
			a_input.write(reg_file.output1);
			b_input.write(reg_file.output2);
			NS = and2;
			break;
		case and2:
			opr_output.write(alu_output);
			NS = fetch1;
			break;
//		case and3:
//			break;
		case or1:
			k3_signal.write(SC_LOGIC_0);
			k2_signal.write(SC_LOGIC_1);
			k1_signal.write(SC_LOGIC_0);
			k0_signal.write(SC_LOGIC_1);
			a_input.write(reg_file.output1);
			b_input.write(reg_file.output2);
			NS = or2;
			break;
		case or2:
			opr_output.write(alu_output);
			NS = fetch1;
			break;
//		case or3:
//			break;
		case xor1:
			k3_signal.write(SC_LOGIC_0);
			k2_signal.write(SC_LOGIC_1);
			k1_signal.write(SC_LOGIC_1);
			k0_signal.write(SC_LOGIC_0);
			a_input.write(reg_file.output1);
			b_input.write(reg_file.output2);
			NS = xor2;
			break;
		case xor2:
			opr_output.write(alu_output);
			NS = fetch1;
			break;
//		case xor3:
//			break;
		case shl1:
			k3_signal.write(SC_LOGIC_1);
			k2_signal.write(SC_LOGIC_0);
			k1_signal.write(SC_LOGIC_0);
			k0_signal.write(SC_LOGIC_0);
			a_input.write(reg_file.output1);
			b_input.write(reg_file.output2);
			NS = shl2;
			break;
		case shl2:
			opr_output.write(alu_output);
			NS = fetch1;
			break;		
		case shr1:
			k3_signal.write(SC_LOGIC_1);
			k2_signal.write(SC_LOGIC_0);
			k1_signal.write(SC_LOGIC_1);
			k0_signal.write(SC_LOGIC_X);
			a_input.write(reg_file.output1);
			b_input.write(reg_file.output2);
			NS = shr2;
			break;
		case shr2:
			opr_output.write(alu_output);
			NS = fecth1;
			break;
		case mov1:
			tmp_val.write(reg_file.output1);
			NS = mov2;
			break;
		case mov2:
			opr_output.write(tmp_val);
			NS = fetch1;
			break;
		case load1:
			reg_file.readEnable.write(SC_LOGIC_1);
			reg_file.r1Code.write(IR.range(20, 16));
			NS = load2;
			break;
		case load2:
			reg_file.read(0);
			MAR.write(reg_file.output1.read());
			NS = load3;
			break;
		case load3:
			// FIXME
			memory.readEnable.write(SC_LOGIC_1);
			memory.address.write(MAR.read());
			//
			MDR.write(memory.output);
			NS = load4;
			break;
		case load4:
			reg_file.readEnable.write(SC_LOGIC_0);
			reg_file.write.write(SC_LOGIC_1);
			reg_file.r1Code.write(IR.range(25, 21));
			reg_file.input.write(MDR.read());
			NS = fetch1;
			break;
		case store1:
			reg_file.readEnable.write(SC_LOGIC_1);
			reg_file.write.write(SC_LOGIC_0);
			reg_file.r1Code.write(IR.range(20, 16));
			NS = store2;
			break;
		case store2:
			reg_file.read(0);
			MDR.write(reg_file.output1);
			NS = store3;
			break;
		case store3:
			reg_file.r1Code.write(IR.range(25, 21));
			reg_file.read(0);
			MAR.write(reg_file.output1);
			NS = store4;
			break;
		case store4:
			// FIXME
			memory.readEnable.write(SC_LOGIC_0);
			memory.writeEnable.write(SC_LOGIC_1);
			//
			NS = fetch1;
			break;
		case loadi:
			reg_file.readEnable.write(SC_LOGIC_1);
			reg_file.write.write(SC_LOGIC_0);
			reg_file.r1Code.write(IR.range(25, 21));
			reg_file.input.write(IR.range(20, 5));
			reg_file.write();
			NS = fetch1;
			break;
		case jump:
			PC.write(IR.range(25, 10));
			NS = fetch1;
			break;
		case beq1:
			reg_file.readEnable.write(SC_LOGIC_1);
			reg_file.write.write(SC_LOGIC_0);
			reg_file.r1Code.write(IR.range(25, 21));
			reg_file.r2Code.write(IR.range(20, 16));
			reg_file.read(0);
			tmp1.write(reg_file.output1);
			tmp2.write(reg_file.output2);
			NS = beq2;
			break;
		case beq2:
			if (tmp1 == tmp2)
				PC = IR.range(15, 0);
			NS = fetch1;
			break;
		case bneq1:
			reg_file.readEnable.write(SC_LOGIC_1);
			reg_file.write.write(SC_LOGIC_0);
			reg_file.r1Code.write(IR.range(25, 21));
			reg_file.r2Code.write(IR.range(20, 16));
			reg_file.read(0);
			tmp1.write(reg_file.output1);
			tmp2.write(reg_file.output2);
			NS = bneq2;
			break;
		case bneq2:
			if (tmp1 != tmp2)
				PC = IR.range(15, 0);
			NS = fetch1;
			break;
	}
}

void Processor::seq () {
	CS = NS;
	memR.write(SC_LOGIC_0);
	memW.write(SC_LOGIC_0);
}
#endif

