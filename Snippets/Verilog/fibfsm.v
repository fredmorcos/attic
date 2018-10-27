module fibfsm (clk, n, out);
   parameter w = 32;
   input [w-1:0]  n;
   input 	  clk;
   output [w-1:0] out;
   
   reg [1:0] 	  state, nstate;

   reg [w-1:0] 	    a, b, i;
   wire [w-1:0]     s, inci;

   parameter S0 = 2'b00;   
   parameter S1 = 2'b01;
   parameter S2 = 2'b10;

   assign s = a + b;
   assign out = s;
   assign inci = i + 1;

   initial begin
      a <= 1;
      b <= 1;
      i <= 3;
      state <= S0;
   end

   always @ (posedge clk) begin
      case (state)
	S0: 
	  begin
	     if (i < n)
	       if (i[0]) /* if (i & 1) */
		 nstate = S1;
	       else
		 nstate = S2;
	     else
	       nstate = S0;
	  end
	S1:
	  begin
	     b = s;
	     i = inci;
	     nstate = S0;
	  end
	S2:
	  begin
	     a = s;
	     i = inci;
	     nstate = S0;
	  end
      endcase // case (state)

      state = nstate;
   end
endmodule // fibfsm

module test;
   parameter w = 32;
   reg [w-1:0]  n;
   reg 		clk;
   wire [w-1:0] out;

   fibfsm #(w) f (clk, n, out);
   
   initial begin
      n = 10;

      $monitor($time, ":%b fib(%d) = %d", clk, n, out);
      clk = 0;
      #40 $finish;
   end

   always
     #1 clk = ~clk;
endmodule