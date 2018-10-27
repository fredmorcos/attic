#include "OR_32_Test.h"

#pragma region Generator
	void Generator::generate(){
		a.write(0x0000ffff);
		b.write(0xffff0000);
		wait();

		a.write(0x00000000);
		b.write(0xffffffff);
		wait();
		
		a.write(0x00000000);
		b.write(0x00000000);
		wait();

		a.write(0xffffffff);
		b.write(0xffffffff);
		wait();

		a.write(0xffff0000);
		b.write(0x0000ffff);
		wait();

		a.write(0x00000000);
		b.write(0x00000001);
		wait();

		sc_stop();
		}//end generate
#pragma endregion

#pragma region Monitor
	void Monitor::print(){
		sc_lv <32> aTemp, bTemp, testResult, moduleResultRead;
		while(true){
			wait();

			aTemp = a.read();
			bTemp = b.read();

			//Software test
			for(int i = 0; i < 32; i++){
				testResult[i] = aTemp[i] | bTemp[i];
				}//end for

			cout << hex << "Module result: \nA: " << a <<"\nB: " << b << "\nResult: " << result <<"\t";
			moduleResultRead = result.read();
			
			if(moduleResultRead == testResult)
				cout << "[Success]\n";
			else
				cout << "[Failed]\n";
			
			}//end while
		}// end print
#pragma endregion