module alu(
	input [15:0] a,
	input [15:0] b,
	input [2:0] control,

	output [15:0] ret
);

always @(*)
begin
	case (control)
		3'b000: ret = a + b;	
		3'b001: ret = a - b;	
		3'b010: ret = ~a;	
		3'b011: ret = a << b;	
		3'b100: ret = a >> b;	
		3'b101: ret = a & b;	
		3'b110: ret = a | b;	
		3'b111: begin if (a <b) ret = 16'd1; else ret = 16'd0; end
	endcase
end


endmodule
