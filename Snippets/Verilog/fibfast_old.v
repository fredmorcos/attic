module register (clk, we, d, q);
   input [31:0] d;
   input 	clk;
   input 	we;
   
   output [31:0] q;
   
   wire 	 clk;
   wire 	 we;
   wire [31:0] 	 d;
   
   reg [31:0] 	 q;
   
   always @ (posedge clk)
     if (we) q <= d;
endmodule // register

module fibfast (clk, n, start, fibn, done);
   input [31:0] n;
   input 	clk;
   input 	start;
   
   output [31:0] fibn;
   output 	 done;

   wire 	 start;
   wire [31:0] 	 n;
   wire 	 clk;
   reg [31:0] 	 fibn;
	 
   reg [31:0] 	 in_n;

   reg 		 done;
   reg [2:0] 	 state;
   reg [2:0] 	 nextstate;

   reg [31:0] 	 b;
   reg [31:0] 	 h;
   reg [31:0] 	 d;
   reg [31:0] 	 f;

   wire [31:0] 	 nextn;
   wire [31:0] 	 nextb;
   wire [31:0] 	 nexth;
   wire [31:0] 	 nextd;
   wire [31:0] 	 nextf;

   assign nextn = in_n / 2;
   assign nextb = (b * f) + (d * f) + (b * h);
   assign nexth = (f * f) + (h * h);
   assign nextd = (b * f) + (d * h);
   assign nextf = (f * f) + (2 * f * h);

   initial begin
      done = 1;
      state = 0;
      in_n = n;
   end
   
   always @ (posedge clk)
     state = nextstate;
   
   always @ (posedge clk)
     if (state == 0 && start) begin
	nextstate = 1;
     end else if (state == 1) begin
	done = 0;
	b = 0;
	h = 0;
	d = 1;
	f = 1;
	nextstate = 2;
     end else if (state == 2) begin
	if (n <= 0) begin
	   nextstate = 0;
	   fibn = b;
	   done = 1;
	end else begin
	   nextstate = 3;
	end
     end else if (state == 3) begin
	if (n & 1 == 1) begin
	   b = nextb;
	   d = nextd;
	end

	f = nextf;
	h = nexth;
	in_n = nextn;

	nextstate = 2;
     end
endmodule // fibfast

module testbench;
   reg clk;
   reg [31:0] n;
   reg 	      start;

   wire [31:0] fibn;
   wire        done;

   fibfast ff (clk, n, start, fibn, done);
   
   initial begin
      clk = 0;
      n = 10;
      start = 1;
      $monitor($time,, "%b %d %b %d %b", clk, n, start, fibn, done);
      #100 $finish();
      
   end

   always
     #10 clk = ~clk;
endmodule // testbench
