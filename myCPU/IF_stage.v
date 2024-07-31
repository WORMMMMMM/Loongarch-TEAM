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
    input  [31:0] inst_sram_rdata,
    output [ 1:0] inst_sram_size,
    input  inst_sram_addr_ok,
    input  inst_sram_data_ok,
    output inst_sram_wr
);

wire br_taken_r;
wire br_taken;
wire br_flush;
wire [31:0] br_target;

wire ps_stall;
wire ps_ready_go;
wire ps_to_fs_valid;

wire fs_flush;
wire fs_stall;
wire fs_ready_go;
wire fs_allowin;
reg  fs_valid;

reg  excp_flush_buf;
reg  ertn_flush_buf;
reg  br_taken_buf;
reg  [31:0] eentry_buf;
reg  [31:0] era_buf;
reg  [31:0] br_target_buf;

wire [31:0] seq_pc;
wire [31:0] nextpc;

reg  [31:0] fs_pc;
reg  [31:0] fs_inst_buf;
reg  fs_inst_buf_valid;
wire [31:0] fs_inst;

wire excp_adef;
wire fs_excp;
wire [15: 0] fs_excp_num;


// pre-IF stage
assign {
    br_taken_r,
    br_target
} = br_bus;
assign br_taken = br_taken_r === 1'bx ? 1'b0 :  br_taken_r;
assign br_flush = br_taken && ds_allowin;

assign ps_stall       = !(inst_sram_req && inst_sram_addr_ok);
assign ps_ready_go    = !ps_stall;
assign ps_to_fs_valid = ps_ready_go;

always @(posedge clk) begin
    if (reset) begin
        excp_flush_buf <= 1'b0;
        ertn_flush_buf <= 1'b0;
        br_taken_buf   <= 1'b0;
    end
    else if (ps_stall) begin
        if (excp_flush) begin
            excp_flush_buf <= 1'b1;
            eentry_buf     <= eentry;
        end
        else if (ertn_flush) begin
            ertn_flush_buf <= 1'b1;
            era_buf        <= era;
        end
        else if (br_flush) begin
            br_taken_buf   <= 1'b1;
            br_target_buf  <= br_target;
        end
    end
    else if (!ps_stall) begin
        excp_flush_buf <= 1'b0;
        ertn_flush_buf <= 1'b0;
        br_taken_buf   <= 1'b0;
    end
end

assign seq_pc = fs_pc + 32'h4;
assign nextpc = excp_flush_buf ? eentry_buf :
                excp_flush     ? eentry :
                ertn_flush_buf ? era_buf :
                ertn_flush     ? era :
                br_taken_buf   ? br_target_buf :
                br_taken       ? br_target :
                seq_pc;

assign inst_sram_req   = fs_allowin;
assign inst_sram_wstrb = 4'b0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0;
assign inst_sram_size  = 2'b10;
assign inst_sram_wr    = 1'b0;


// IF stage
assign fs_flush       = excp_flush || ertn_flush || br_flush;
assign fs_stall       = !fs_inst_buf_valid;
assign fs_ready_go    = !fs_flush && !fs_stall;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin || fs_flush;
assign fs_to_ds_valid = fs_valid && fs_ready_go;
always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= ps_to_fs_valid;
    end

    if (reset) begin
        fs_pc <= 32'h1bfffffc;  //trick: to make nextpc be 0xbfc00000 during reset
    end
    else if (ps_to_fs_valid && fs_allowin) begin
        fs_pc <= nextpc;
    end
end

always @(posedge clk) begin
    if (reset) begin
        fs_inst_buf <= 32'b0;
        fs_inst_buf_valid <= 1'b0;
    end
    else if(fs_ready_go && ds_allowin) begin
        fs_inst_buf_valid <= 1'b0;
    end
    else if (inst_sram_data_ok) begin
        fs_inst_buf <= inst_sram_rdata;
        fs_inst_buf_valid <= 1'b1;
    end
end

assign fs_inst = fs_inst_buf;

assign excp_adef = nextpc[1:0] != 2'b00;
assign fs_excp     = excp_adef;
assign fs_excp_num = {1'b0, excp_adef, 14'b0};

assign fs_to_ds_bus = {
    fs_pc,
    fs_inst,
    fs_excp,
    fs_excp_num
};


reg  mid_handshake;
always @(posedge clk) begin
    if (reset)
        mid_handshake <= 1'b0;
    else if (inst_sram_data_ok)
        mid_handshake <= 1'b0;
    else if (inst_sram_req && inst_sram_addr_ok)
        mid_handshake <= 1'b1;
end

endmodule
