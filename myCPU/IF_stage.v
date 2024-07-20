`include "myCPU.h"

module if_stage(
    input  clk,
    input  reset,
    input  ds_allowin,
    // from ws
    input  [`WS_TO_FS_BUS_WD-1:0] ws_to_fs_bus,
    // to ds
    output fs_to_ds_valid,
    output [`FS_TO_DS_BUS_WD-1:0] fs_to_ds_bus,
    input  [`BR_BUS_WD-1:0] br_bus,
    input  excp_flush,
    input  ertn_flush,
    input  [31:0] era,
    input  [31:0] eentry,
    // inst sram interface
    output        inst_sram_req  ,
    output [ 3:0] inst_sram_wstrb,
    output [ 1:0] inst_sram_size,
    output        inst_sram_en   ,
    output [ 3:0] inst_sram_we  ,
    output [31:0] inst_sram_addr ,
    output [31:0] inst_sram_wdata,
    input  [31:0] inst_sram_rdata
);

wire br_taken;
wire br_taken_r;
wire [31:0] br_target;

wire to_fs_valid;
reg  fs_valid;
wire fs_ready_go;
wire fs_allowin;

wire [31: 0] seq_pc;
wire [31: 0] nextpc;

reg  [31: 0] fs_pc;
wire [31: 0] fs_inst;

wire flush;

wire excp_adef;

wire pfs_excp;
wire [15: 0] pfs_excp_num;
wire fs_excp;
wire [15: 0] fs_excp_num;

assign inst_sram_req   = fs_allowin; //req

// pre-IF stage
assign {br_taken_r, br_target} = br_bus;
assign br_taken = br_taken_r !== 1'bx ? br_taken_r : 1'b0;

assign to_fs_valid = ~reset;
assign seq_pc      = fs_pc + 3'h4;
assign nextpc      = excp_flush ? eentry :
                     ertn_flush ? era :
                     br_taken   ? br_target :
                     seq_pc;

assign flush = excp_flush | ertn_flush;

assign excp_adef   = nextpc[1:0] != 2'b00;

assign pfs_excp     = excp_adef;
assign pfs_excp_num = {1'b0, excp_adef, 14'b0};

// IF stage
assign fs_ready_go    = 1'b1;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;
assign fs_to_ds_valid =  fs_valid && fs_ready_go;

always @(posedge clk) begin
    if (reset || flush) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= to_fs_valid;
    end

    if (reset) begin
        fs_pc       <= 32'h1bfffffc;  //trick: to make nextpc be 0xbfc00000 during reset
        fs_excp     <= 1'b0;
        fs_excp_num <= 1'b0;
    end
    else if (to_fs_valid && fs_allowin) begin
        fs_pc       <= nextpc;
        fs_excp     <= pfs_excp;
        fs_excp_num <= pfs_excp_num;
    end
end

assign inst_sram_size  = 2'b10;
assign inst_sram_en    = to_fs_valid && fs_allowin;
assign inst_sram_we    = 4'h0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0;

assign inst_sram_wstrb = 4'b0;

assign fs_inst         = br_taken ? 32'h02800000 : inst_sram_rdata;

assign fs_to_ds_bus[31: 0] = fs_pc;
assign fs_to_ds_bus[63:32] = fs_inst;
assign fs_to_ds_bus[64:64] = fs_excp;
assign fs_to_ds_bus[80:65] = fs_excp_num;

endmodule
