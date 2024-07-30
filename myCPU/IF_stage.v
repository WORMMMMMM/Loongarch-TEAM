`include "myCPU.h"

module if_stage(
    input  clk,
    input  reset,

    // allowin
    input  ds_allowin,

    // to ds
    output fs_to_ds_valid,
    output [`FS_TO_DS_BUS_WD-1:0] fs_to_ds_bus,

    input  [`BR_BUS_WD-1:0] br_bus,

    input  excp_flush,
    input  ertn_flush,
    input  [31:0] era,
    input  [31:0] eentry,

    // inst sram interface
    output inst_sram_req,
    output [ 3:0] inst_sram_wstrb,
    output [31:0] inst_sram_addr,
    output [31:0] inst_sram_wdata,
    input  [31:0] inst_sram_rdata
    output [ 1:0] inst_sram_size,
    input  inst_sram_addr_ok,
    input  inst_sram_data_ok,
    output inst_sram_wr,
);

wire br_taken_r;
reg  br_taken_buf;
wire br_taken;
wire br_flush;
wire br_stall;
wire [31:0] br_target;

wire ps_ready_go;
wire to_fs_valid;

wire fs_flush;
wire fs_stall;
wire fs_ready_go;
wire fs_allowin;
reg  fs_valid;

wire [31:0] seq_pc;
reg  [31:0] nextpc_buf;
wire [31:0] nextpc;

reg  [31:0] fs_pc;
reg  [31:0] fs_inst_buf;
wire [31:0] fs_inst;
reg  fs_inst_buf_valid;
reg  mid_handshake;

wire excp_adef;
wire fs_excp;
wire [15: 0] fs_excp_num;


// pre-IF stage
assign {
    br_taken_r,
    br_stall,
    br_target
} = br_bus;
assign br_taken = br_taken_r === 1'bx ? 1'b0 :  br_taken_r;
assign br_flush = br_taken && ds_allowin;

assign ps_ready_go = inst_sram_req && inst_sram_addr_ok;
assign to_fs_valid = ps_ready_go;

assign seq_pc = fs_pc + 32'h4;
assign nextpc = excp_flush   ? eentry :
                ertn_flush   ? era :
                br_taken_buf ? nextpc_buf :
                br_taken     ? br_target :
                seq_pc;


// IF stage
assign fs_flush       = excp_flush || ertn_flush || br_flush;
assign fs_stall       = !inst_sram_data_ok && !fs_inst_buf_valid;
assign fs_ready_go    = !fs_flush && !fs_stall;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin || fs_flush;
assign fs_to_ds_valid = fs_valid && fs_ready_go;
always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= to_fs_valid;
    end
end
always @(posedge clk) begin
    if (reset) begin
        fs_pc       <= 32'h1bfffffc;  //trick: to make nextpc be 0xbfc00000 during reset
    end
    else if (to_fs_valid && fs_allowin) begin
        fs_pc       <= nextpc;
    end
end

always @(posedge clk) begin
    if (reset)
        mid_handshake <= 1'b0;
    else if (inst_sram_data_ok)
        mid_handshake <= 1'b0;
    else if (inst_sram_req && inst_sram_addr_ok)
        mid_handshake <= 1'b1;
end

always @(posedge clk) begin
    if(reset) begin
        fs_inst_buf <= 32'b0; 
    end
    else if(inst_sram_data_ok && ~ds_allowin) begin
        fs_inst_buf <= inst_sram_rdata;
    end
end

always @(posedge clk) begin
    if(reset) begin
        fs_inst_buf_valid <= 1'b0;
    end
    else if(fs_ready_go && ds_allowin) begin
        fs_inst_buf_valid <= 1'b0;
    end
    else if(inst_sram_data_ok && ~ds_allowin) begin
        fs_inst_buf_valid <= 1'b1;
    end
end

always @(posedge clk) begin
    if(reset) begin
        nextpc_buf <= 32'b0;
    end
    else if(br_taken && ~br_stall) begin
        nextpc_buf <= nextpc;
    end
end

always @(posedge clk)begin
    if(reset) begin
        br_taken_buf <= 1'b0;
    end
    else if(br_taken_buf && inst_sram_req && inst_sram_addr_ok && fs_allowin) begin
        br_taken_buf <= 1'b0;
    end 
    else if(br_taken && ~br_stall && ~(inst_sram_req && inst_sram_addr_ok)) begin
        br_taken_buf <= br_taken;
    end
end

assign inst_sram_req   = fs_allowin && ~br_stall;
assign inst_sram_wstrb = 4'b0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0;
assign inst_sram_size  = 2'b10;
assign inst_sram_wr    = 1'b0;

assign fs_inst = fs_inst_buf_valid ? fs_inst_buf : inst_sram_rdata;

assign excp_adef = nextpc[1:0] != 2'b00;
assign fs_excp     = excp_adef;
assign fs_excp_num = {1'b0, excp_adef, 14'b0};

assign fs_to_ds_bus = {
    fs_pc,
    fs_inst,
    fs_excp,
    fs_excp_num
};

endmodule
