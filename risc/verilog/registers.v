
module registers (
	input clk,
	input reg_write_e,
	input [3:0] reg_write_dest,
	input [15:0] reg_write_data,

	input[3:0] reg_read_addr,
	output [15:0] reg_read_data
);

reg [15:0] reg_array[15:0];
integer i;

initial begin
	for (i=0; i < 16; i=i+1)
		reg_array[i] <= 16'd0;
end

always @(posedge clk) begin

	if (reg_write_e) begin
		reg_array[reg_write_dest] <= reg_write_data;
	end

end

assign reg_read_data = reg_array[reg_read_addr];

endmodule
