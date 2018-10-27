#include <systemc.h>

SC_MODULE (Memory) {
    sc_in<sc_lv<16> > addr;
    sc_in<sc_logic> read, write;

    sc_inout_lv<32> data;

    sc_lv<8> Memory_data[65536];    /* 2^16 items in mem array */

    SC_CTOR(Memory) {
        SC_METHOD(run);
        sensitive << addr << read << write;
    }
};

void Memory::run () {

}

