#ifndef Reg_File
#define Reg_File

#include <systemc.h>
#include "Mux_5_1.h"
#include "Mux_2_1.h"
#include "And_1.h"

SC_MODULE(Register_File){
	
	//Ports
	sc_in <bool> readEnable, write;					//writeEnable
	//sc_in <bool> clk;								//optional clock
	sc_in < sc_lv <5> > r1Code, r2Code;				//register number
	sc_in < sc_lv <32> > input;						//input (for writing)
	sc_out <sc_lv <32> > output1, output2;			//value stored in the register

	//Register
	sc_signal <sc_lv <32> > r0, r1, r2, r3, r4;

	void read(){	//Always reads 2 registers
		if(readEnable){
			//First output
			if(r1Code.read()[0] == 1)
				output1.write(r0);

			else if(r1Code.read()[1] == 1)
				output1.write(r1);

			else if(r1Code.read()[2] == 1)
				output1.write(r2);

			else if(r1Code.read()[3] == 1)
				output1.write(r3);

			else if(r1Code.read()[4] == 1)
				output1.write(r4);

			//Second output
			if(r2Code.read()[0] == 1)
				output2.write(r0);

			else if(r2Code.read()[1] == 1)
				output2.write(r1);

			else if(r2Code.read()[2] == 1)
				output2.write(r2);

			else if(r2Code.read()[3] == 1)
				output2.write(r3);

			else if(r2Code.read()[4] == 1)
				output2.write(r4);
		}//end if
	}//end read

	void writeToRegFile(){
		if(write){
			if(r1Code.read()[0] == 1)
				r0.write(input);

			else if(r1Code.read()[1] == 1)
				r1.write(input);

			else if(r1Code.read()[2] == 1)
				r2.write(input);

			else if(r1Code.read()[3] == 1)
				r3.write(input);

			else if(r1Code.read()[4] == 1)
				r4.write(input);
		}//end if
	}//end write

	//Constructor
	SC_CTOR(Register_File){
		SC_METHOD(read);
		sensitive << r1Code << r2Code << readEnable;
		//sensitive << clk.pos();

		SC_METHOD(writeToRegFile);
		sensitive << r1Code << r2Code << write;
		//sensitive << clk.pos();

		//Init registers with 0's
		r0.write(0x0);
		r1.write(0x0);
		r2.write(0x0);
		r3.write(0x0);
		r4.write(0x0);

	}//end constructor
};
#endif
