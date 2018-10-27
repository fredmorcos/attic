#include "Inverter_32.h"

void Inverter_32::invert() {

	sc_lv <32> inputTemp, outTmp;
	inputTemp = input.read();

	for (int i = 0; i < 32; i++){
		if(enable == 1)
			outTmp[i] = !(inputTemp.get_bit(i));
		else 
			outTmp[i] = inputTemp.get_bit(i);
		}//end for
	
	out.write(outTmp);
}

