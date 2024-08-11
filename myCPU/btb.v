module btb
#(
    parameter BTBNUM = 32,
    parameter RASNUM = 16
)
(
    input            clk,
    input            reset,
    //from/to if
    input   [31:0]  fetch_pc,
    input           fetch_en,
    output          ret_en,
    output  [31:0]  ret_pc,
    output          taken,
    output  [4:0]   ret_index,

    //from id
    input           operate_en,
    input   [31:0]  operate_pc,
    input   [4:0]   operate_index,
    input           pop_ras,
    input           push_ras,
    input           add_entry,
    input           delete_entry,
    input           pre_error,
    input           pre_right,
    input           target_error,
    input           right_orien,
    input           right_target
);

reg [29:0]  btb_pc       [BTBNUM-1:0];
reg [29:0]  btb_target   [BTBNUM-1:0];
reg [ 1:0]   btb_counter  [BTBNUM-1:0];
reg [BTBNUM-1:0] btb_valid;

reg [29:0]  ras          [7:0];
reg [ 3:0]  ras_ptr;

reg [29:0]  ras_pc       [RASNUM-1:0];
reg [RASNUM-1:0] ras_valid;

reg         fetch_en_r;
reg [31:0]  fetch_pc_r;

wire [31:0] btb_match_rd;
wire [15:0] ras_match_rd;
wire [4:0]  btb_match_index;
wire [3:0]  ras_match_index;

always @(posedge clk) begin
    if (reset)
        fetch_en_r <= 1'b0;
    else
        fetch_en_r <= fetch_en;

    if (fetch_en)
        fetch_pc_r <= fetch_pc;
end

genvar i;
generate
    for (i = 0; i < BTBNUM; i = i + 1) 
        begin: btb_match
            assign btb_match_rd[i] = fetch_en_r && ((fetch_pc_r[31:2] == btb_pc[i][29:0]) && btb_valid[i]);
        end
endgenerate

generate
    for (i = 0; i < BTBNUM; i = i + 1)
        begin: ras_match
            assign ras_match_rd[i] = fetch_en_r && ((fetch_pc_r[31:2] == ras_pc[i][29:0]) && ras_valid[i]);
        end
endgenerate

assign ras_top = ras[ras_ptr - 4'b1];

encoder_32_5 enc_btb_match_rd(
    .in(btb_match_rd),
    .out(btb_match_index)
);

encoder_16_4 enc_ras_match_rd(
    .in(ras_match_rd),
    .out(ras_match_index)
);
    
assign ret_en = (|btb_match_rd) || (|ras_match_rd);

assign ret_pc = (|btb_match_rd) ? {btb_target[btb_match_index], 2'b0} : (|ras_match_rd) ? {ras_top, 2'b0} : 32'b0;

endmodule