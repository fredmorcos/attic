module y1 (a, b, c, y);
   input a, b, c;
   output y;

   assign y = ~(a & b) & (a & b | a & c);
endmodule // y1

module y2 (a, b, c, y);
   input a, b, c;
   output y;

   assign y = a & ~b & c;
endmodule // y2

module test;
   reg  [3:0] in;
   wire       out1, out2;

   y1 m1 (in[2], in[1], in[0], out1);
   y2 m2 (in[2], in[1], in[0], out2);

   initial begin
      $display("a b c y1 y2");
      for (in = 0; in < 8; in = in + 1) begin
	 #1 $display("%d %d %d  %d  %d",
		     in[2], in[1], in[0],
		     out1, out2);
      end
   end
endmodule // test
