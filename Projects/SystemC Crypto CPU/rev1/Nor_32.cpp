#include "Nor_32.h"

void Nor_32::exec(){

	sc_lv <32> input = a.read();
	sc_logic tempOutput;
	sc_logic invertedOutput;

	tempOutput = input[0] | input[1];

	for(int i = 2; i < 32; i++){
		tempOutput = tempOutput | input[i];
		}
	invertedOutput = !tempOutput.value();
	result.write(invertedOutput);
	}