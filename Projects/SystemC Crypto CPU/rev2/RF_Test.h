#ifndef RFTEST
#define RFTEST

#include <systemc.h>

SC_MODULE (RF_Gen) {
	
	sc_in <bool> clk;
	//sc_out < sc_lv<32> > regOut1, regOut2;
	sc_out < sc_logic > write;
	sc_out < sc_lv <5> > r1Code, r2Code;
	sc_out < sc_lv <32> > inputData;

	void gen() {
		//Store data
		write.write(SC_LOGIC_1);
		r1Code.write(0x0);
		r2Code.write(0x1);
		inputData.write(0xFFFF);
		wait();
		
		//Read
		write.write(SC_LOGIC_0);
		r1Code.write(0x0);
		r2Code.write(0x1);
		wait();
		
		sc_stop();
	}//end gen()

	SC_CTOR (RF_Gen) {
		SC_THREAD (gen);
		sensitive << clk.pos();
	}
};

SC_MODULE (RF_Mon) {
	sc_in <bool> clk;
	sc_in < sc_logic > write;
	sc_in < sc_lv <5> > r1Code, r2Code;
	sc_in < sc_lv <32> > inputData, output1, output2;
	
	void check() {
		cout << "Register File Testbench" << endl;
		cout << "R1\t\tR2\t\tWrite enable\t\tinput data\t\tout1\t\tout2" << endl;
		
		while (1) {
			wait();
			cout << r1Code << "\t" << r2Code << "\t" << write << "\t" << inputData << "\t" << output1 << "\t\t" << output2 << endl;
		}
	}//end check
	
	SC_CTOR(RF_Mon) {
		SC_THREAD(check);
		sensitive << clk.pos();
	}
};

#endif