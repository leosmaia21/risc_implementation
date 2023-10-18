module data(input clk,
		input[2:0] mem_addr,
		input[15:0] mem_write_data,
		input mem_write,
		input mem_read,
		output[15:0] mem_read_data
);

reg[15:0] memory [6:0];


initial begin
	$readmemb("../datamemory", memory);
end

always @(posedge clk) begin
	if (mem_write)
		memory[mem_addr] <= mem_read_data;

end

assign mem_read_data = (mem_read == 1'b1) ? memory[mem_addr] : 16'd0;

endmodule
