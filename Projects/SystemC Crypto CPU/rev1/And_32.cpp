#include "And_32.h"

#pragma region AND_32

void And_32::exec(){
	int i;
	sc_lv <32> aTemp, bTemp, resultTemp;
	
	//Read input
	aTemp = a.read();
	bTemp = b.read();
	
	for(i = 0; i < 32; i++){
		resultTemp[i] = aTemp[i] & bTemp[i];
		}//end for loop

	//Write output
	result.write(resultTemp);
	}//end and method

#pragma endregion