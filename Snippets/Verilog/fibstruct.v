module fib (clk, n, out);
   parameter w = 32;
   input            clk;
   input [w - 1:0]  n;
   output [w - 1:0] out;

   reg [w - 1:0]    a, b, i;
   wire [w - 1:0]   s, inci;
   
   assign s = a + b;
   assign out = s;
   assign inci = i + 1;

   initial begin
      a <= 1;
      b <= 1;
      i <= 3;
   end
   
   always @ (posedge clk) begin
      if (i < n) begin
	 if (i[0]) /* if (i & 1) { */
	   b <= s;
	 else
	   a <= s;
	 i <= inci;
      end
   end
endmodule // fib

module test;
   parameter w = 32;
   reg [w - 1:0]  n;
   reg 		  clk;
   wire [w - 1:0] out;

   fib #(w) f (clk, n, out);
   
   initial begin
      n = 10;

      $monitor($time, ":%b fib(%d) = %d", clk, n, out);
      clk = 0;

      #20 $finish;
   end

   always
     #1 clk = ~clk;
endmodule
