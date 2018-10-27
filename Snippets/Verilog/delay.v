module delay_demo (a, b, y);
   input a, b;
   output y;

   assign #5 y = a | b;
endmodule // delay_demo

module test;
   reg  a, b;
   wire y;

   delay_demo m (a, b, y);

   initial begin
      $monitor($time, ": a = %d b = %d y = %d", a, b, y);
      a = 0; b = 0;
      #10 a = 1;
      #10 b = 1;
      #10 a = 0;
      #10 b = 0;
   end
endmodule // test
