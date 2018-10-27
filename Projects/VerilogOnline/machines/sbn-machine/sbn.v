module mem (clk, wen, addr, in, out);
   parameter                  data_width = 32;
   parameter                  addr_width = 8;
   input                      clk;
   input                      wen;
   input     [addr_width-1:0] addr;
   input     [data_width-1:0] in;
   output    [data_width-1:0] out;
   reg 	     [data_width-1:0] m [0:(2**addr_width)-1];
   
   always @ (posedge clk)
     if (wen) m[addr] <= in;

   always @ (addr)
     assign out = m[addr];
endmodule // mem

module mux2to1 (clk, sel, d1, d0, out);
   parameter width = 32;
   input clk;
   input sel;
   input [width-1:0] d1;
   input [width-1:0] d0;
   output [width-1:0] out;

   always @ (posedge clk)
     case (sel)
       1: out <= d1;
       0: out <= d0;
       default: $display("Something is awfully wrong in the mux2to1");
     endcase // case (sel)
endmodule // mux2to1

module mux4to1 (clk, sel, d3, d2, d1, d0, out);
   parameter width = 32;
   input clk;
   input [1:0] sel;
   input [width-1:0] d3;
   input [width-1:0] d2;
   input [width-1:0] d1;
   input [width-1:0] d0;
   output [width-1:0] out;

   always @ (posedge clk)
     case (sel)
       3: out <= d3;
       2: out <= d2;
       1: out <= d1;
       0: out <= d0;
       default: $display("Something is awfully wrong in the mux4to1");
     endcase // case (sel)
endmodule // mux4to1

module control (clk);
   parameter data_width = 32;
   parameter addr_width = 8;
   
   wire imem_en; assign imem_en = 0;
   wire [addr_width-1:0] imem_addr;
   wire [data_width-1:0] imem_din;
   wire [data_width-1:0] imem_dout;
   mem imem (clk, imem_en, imem_addr, imem_din, imem_dout);

   wire dmem_en;
   wire [addr_width-1:0] dmem_addr;
   wire [data_width-1:0] dmem_din;
   wire [data_width-1:0] dmem_dout;
   mem imem (clk, dmem_en, dmem_addr, dmem_din, dmem_dout);

   reg [addr_width-1:0]  pc;
   // mux2to1 pc_mux (clk, SELECT, d, pc + 1, pc);
endmodule // control
