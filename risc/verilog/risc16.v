`include "registers.v"

module Instruction_memory( input[15:0] pc, 
	output[15:0] instruction
);

reg [16 - 1:0] memory [15 - 1:0];

initial
begin
	$readmemb("../codeinstructions", memory, 0 ,14);
end
// need to shift beacause the code comes dont come in bytes
assign instruction = memory[pc[3:0] >> 1];

endmodule


module risc16;

reg [15:0] pc;
reg clk;
integer count;

wire [15:0] instruction;

Instruction_memory inst_memory (
	.pc(pc),
	.instruction(instruction)
);

initial begin
	clk = 0;
	pc = 0;
	count = 0;
end

always #5 clk = ~clk;

always @(posedge clk) begin
	if (pc <= 6) 
		$display("Read instruction from address %h: %h", pc, instruction);
	pc <= pc + 2;
	count <= count + 1;

end

initial begin
	#100;
	$display("adios, %d", count);
	
	$finish(0);
end
endmodule
