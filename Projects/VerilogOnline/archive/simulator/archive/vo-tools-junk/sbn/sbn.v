/*
 * SBN machine with hardwired FSM control
 * (c) Volker Strumpen
 * 
 * modified to halt execution 
 * if result address is C all fff's 
 * i.e. ff for fwidth=8
 * by Martin Polak
 */
module sbn (clk, state, PC, a, b);
   parameter fwidth = 8;   // field width of sbn operand
   parameter dwidth = 32;
   input               clk;
   output [2:0]        state;
   output [fwidth-1:0] PC;
   output [dwidth-1:0] a, b;

   parameter iwidth = 4 * fwidth; 
   
   reg [iwidth-1:0]    imem[0:((1<<fwidth)-1)];
   reg [dwidth-1:0]    dmem[0:((1<<fwidth)-1)];

   reg [dwidth-1:0]    X, Y;
   reg [fwidth-1:0]    PC;
   reg [iwidth-1:0]    IR;

   wire [iwidth-1:0]   insn;
   wire [dwidth-1:0]   data, asubb;
   wire [fwidth-1:0]   addr, PCp1, A, B, C, D;
   wire 	       altb, stp;
   reg [1:0] 	       da;
   
   reg [2:0] 	       state, nextstate;
   
   parameter S0 = 3'b000;
   parameter S1 = 3'b001;
   parameter S2 = 3'b010;
   parameter S3 = 3'b011;
   parameter S4 = 3'b100;
   parameter S5 = 3'b101;
   parameter S6 = 3'b111;

   // datapath
   assign insn  = imem[PC];
   assign data  = dmem[addr];
   assign a     = X;   // for monitoring
   assign b     = Y;   // for monitoring
   
   assign asubb = X - Y;
   assign altb  = asubb[dwidth-1];

   
   assign PCp1  = PC + 1;
   assign A     = IR[(4*fwidth-1):(3*fwidth)];
   assign B     = IR[(3*fwidth-1):(2*fwidth)];
   assign C     = IR[(2*fwidth-1):fwidth];
   assign D     = IR[fwidth-1:0];         

   assign stp   = (C == ~{fwidth{1'b0}}) ? 1 : 0;
   assign addr  = (da == 2'b00) ? A : ((da == 2'b01) ? B : C);
   
   always @ (posedge clk)
     case (state) // action at end of state cycle
       S0: begin
          IR <= insn;
 	  da <= 2'b00;
       end
       S1: begin
	  X  <= data;
 	  da <= 2'b01;
       end
       S2: begin
          Y  <= data;
          da <= 2'b10;
       end
       S3: begin
	  dmem[addr] <= asubb;
	  $display("mw:DMEM,%h,%h", addr, asubb);
       end
       S4: PC <= D;
       S5: PC <= PCp1;
       S6: begin
	  // $display("program caused halt with value %d\n",asubb);
	  $finish;
       end
     endcase       

   // state register
   always @ (posedge clk)
     state <= nextstate;

   // next state logic
   always @ (state or altb or stp)
     case (state)
       S0:           nextstate = S1;
       S1:           nextstate = S2;
       S2: if (stp ) nextstate = S6;
       else          nextstate = S3;
       S3: if (altb) nextstate = S4;
       else          nextstate = S5;
       default:      nextstate = S0;
     endcase
   
   initial begin
      $readmemh(%PROG%, imem);
      $readmemh(%DATA%, dmem);
      
      PC = 0;
      state = 0;

      $monitor("%d:%b:%h,%h,%h,%h,%h,%h,%h,%h,%h,%h,%h,%h",
	       $time, clk, PC, X, Y, A, B, C, D, insn, addr, asubb, PCp1, PC);
   end // initial begin
endmodule

module top;
   parameter fwidth = 8;   // field width of sbn operand
   parameter dwidth = 32;

   parameter maximum = 200;
   parameter maxmone = maximum - 1;
   parameter step = 10;
   
   reg clk;
   wire [2:0] state;
   wire [fwidth-1:0] pc;
   wire [dwidth-1:0] a, b;   
   
   sbn #(fwidth,dwidth) mach1 (clk, state, pc, a, b);

   initial begin
      $display("=== start ===");
      
      clk = 0;

      #maxmone $display("=== end ===");
      #1 $finish;
   end
   
   always
     #step clk = ~clk;
endmodule   
