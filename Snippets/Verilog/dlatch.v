module dlatch (clk, d, q);
   input  clk, d;
   output q;
   reg 	  q;

   always @ (clk or d)
     if (clk)
       q <= d;
endmodule // dlatch

module test;
   reg clk, d;
   wire q;

   dlatch m (clk, d, q);
   
   initial begin
      $monitor ($time, ": clk = %d d = %d q = %d", clk, d, q);
      clk = 0; d = 0;
      #5 d = 1;
      #10 clk = 1;
      #5 d = 0;
      #10 clk = 0;
   end
endmodule // test
