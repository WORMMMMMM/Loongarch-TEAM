`include "myCPU.h"

module exe_stage(
    input  clk,
    input  reset,

    // allowin
    input  ms_allowin,
    output es_allowin,

    // from ds
    input  ds_to_es_valid,
    input  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus,

    // to ms
    output es_to_ms_valid,
    output [`ES_TO_MS_BUS_WD-1:0] es_to_ms_bus,

    // forward to ds
    output [`ES_FORWARD_WD-1:0] es_forward,

    // mul div
    output es_div_enable,
    output es_mul_div_sign,
    output [31:0] es_rj_value,
    output [31:0] es_rkd_value,
    input  div_complete,
    
    // data sram interface
    output data_sram_req,
    output [ 3:0] data_sram_wstrb,
    output [31:0] data_sram_addr,
    output [31:0] data_sram_wdata,
    output [ 1:0] data_sram_size,
    input  data_sram_addr_ok,
    output data_sram_wr,

    input  excp_flush,
    input  ertn_flush,
    input  ms_ex,
    input  ws_ex
);

/* --------------  Handshaking  -------------- */
wire es_flush;
wire es_stall;
wire es_ready_go;
reg  es_valid;
reg  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus_r;

wire [31:0] es_pc;
wire        es_op_ld_b;
wire        es_op_ld_h;
wire        es_op_ld_w;
wire        es_op_st_b;
wire        es_op_st_h;
wire        es_op_st_w;
wire        es_op_ld_bu;
wire        es_op_ld_hu;
wire        es_op_rdcntvl_w;
wire        es_op_rdcntvh_w;
wire        es_op_ertn;
wire [31:0] es_imm;
wire [31:0] es_rj_value;
wire [31:0] es_rkd_value;
wire        es_src1_is_pc;
wire        es_src2_is_imm;
wire        es_src2_is_4;
wire [13:0] es_alu_op;
wire        es_mem_en;
wire        es_mem_we;
wire [ 4:0] es_dest;
wire        es_gr_we;
wire        es_res_from_cnt;
wire        es_res_from_mem;
wire        es_res_from_csr;
wire [ 3:0] es_mul_div_op;
wire        es_mul_div_sign;

wire [31:0] es_alu_src1;
wire [31:0] es_alu_src2;
wire        es_div_enable;
wire        es_mul_enable;
wire        es_div_stall;
wire [31:0] es_alu_result;
wire [31:0] es_final_result;

// wire [ 3:0] data_sram_wstrb_sp;

/* timer */
reg  [63:0] timer;
wire [31:0] timer_value;

/* exception */
wire [31:0] err_addr;
wire excp_ale;
wire ds_excp;
wire es_excp;
wire [15:0] ds_excp_num;
wire [15:0] es_excp_num;

/* csr */
wire csr_we;
wire [13:0] csr_num;
wire [31:0] csr_wmask;
wire [31:0] csr_wdata;


assign es_flush       = excp_flush || ertn_flush;
assign es_stall       = es_div_stall || !data_sram_addr_ok && es_mem_en;
assign es_ready_go    = !es_flush && !es_stall;
assign es_allowin     = !es_valid || es_ready_go && ms_allowin || es_flush;
assign es_to_ms_valid = es_valid && es_ready_go;
always @(posedge clk) begin
    if (reset) begin
        es_valid <= 1'b0;
    end
    else if (es_allowin) begin
        es_valid <= ds_to_es_valid;
    end

    if (ds_to_es_valid && es_allowin) begin
        ds_to_es_bus_r <= ds_to_es_bus;
    end
end

// DS to ES bus
assign {
    es_pc,
    es_op_ld_b,
    es_op_ld_h,
    es_op_ld_w,
    es_op_st_b,
    es_op_st_h,
    es_op_st_w,
    es_op_ld_bu,
    es_op_ld_hu,
    es_op_rdcntvl_w,
    es_op_rdcntvh_w,
    es_op_ertn,
    es_imm,
    es_rj_value,
    es_rkd_value,
    es_src1_is_pc,
    es_src2_is_imm,
    es_src2_is_4,
    es_alu_op,
    es_mem_en,
    es_mem_we,
    es_dest,
    es_gr_we,
    es_res_from_cnt,
    es_res_from_mem,
    es_res_from_csr,
    es_mul_div_op,
    es_mul_div_sign,
    ds_excp,
    ds_excp_num,
    csr_we,
    csr_num,
    csr_wmask,
    csr_wdata
} = ds_to_es_bus_r;

/* --------------  ALU interface  -------------- */
assign es_alu_src1 = es_src1_is_pc  ? es_pc[31:0] :
                     es_rj_value;
assign es_alu_src2 = es_src2_is_4   ? 32'h0004 :
                     es_src2_is_imm ? es_imm :
                     es_rkd_value;
assign es_div_enable = (es_mul_div_op[2] | es_mul_div_op[3]) & es_valid;
assign es_mul_enable = (es_mul_div_op[0] | es_mul_div_op[1]) & es_valid;
assign es_div_stall  = es_div_enable & ~div_complete;

alu u_alu(
    .alu_op      (es_alu_op    ),
    .alu_src1    (es_alu_src1  ),
    .alu_src2    (es_alu_src2  ),
    .alu_result  (es_alu_result)
);

assign es_final_result = es_alu_result;

/* --------------  MEM write interface  -------------- */
assign es_addr00 = data_sram_addr[1:0] == 2'b00;
assign es_addr01 = data_sram_addr[1:0] == 2'b01;
assign es_addr10 = data_sram_addr[1:0] == 2'b10;
assign es_addr11 = data_sram_addr[1:0] == 2'b11;
assign data_sram_wstrb_sp= {4{es_op_st_b && es_addr00}} & 4'b0001 |
                           {4{es_op_st_b && es_addr01}} & 4'b0010 |
                           {4{es_op_st_b && es_addr10}} & 4'b0100 |
                           {4{es_op_st_b && es_addr11}} & 4'b1000 |
                           {4{es_op_st_h && es_addr00}} & 4'b0011 |
                           {4{es_op_st_h && es_addr10}} & 4'b1100 |
                           {4{es_op_st_w}}              & 4'b1111;


assign data_sram_req   = es_valid && ms_allowin && es_mem_en && !es_ex && !ms_ex && !ws_ex;
assign data_sram_wstrb = es_mem_we ? data_sram_wstrb_sp : 4'h0;
assign data_sram_addr  = es_final_result;
assign data_sram_wdata = {32{es_op_st_b}} & {4{es_rkd_value[ 7:0]}} |
                         {32{es_op_st_h}} & {2{es_rkd_value[15:0]}} |
                         {32{es_op_st_w}} & es_rkd_value[31:0];
assign data_sram_size  = {2{es_op_st_b || es_op_ld_b || es_op_ld_bu}} & 2'b00 |
                         {2{es_op_st_h || es_op_ld_h || es_op_ld_hu}} & 2'b01 |
                         {2{es_op_st_w || es_op_ld_w}}                & 2'b10;
assign data_sram_wr    = |data_sram_wstrb;

always @(posedge clk) begin
    if (reset) begin
        timer <= 64'h0;
    end
    else begin
        timer <= timer + 64'h1;
    end
end
assign timer_value = es_op_rdcntvl_w ? timer[31: 0] :
                     es_op_rdcntvh_w ? timer[63:32] :
                     32'h0;

assign excp_ale = (es_op_ld_h | es_op_st_h | es_op_ld_hu) & data_sram_addr[0] != 1'b0
                | (es_op_ld_w | es_op_st_w) & data_sram_addr[1:0] != 2'b00;
assign es_excp     = ds_excp | excp_ale;
assign es_excp_num = ds_excp_num | {9'b0, excp_ale, 6'b0};

assign err_addr = data_sram_addr;

assign es_ex = es_valid && (es_excp || es_op_ertn);

assign es_forward = {
    es_valid,
    es_gr_we,
    es_dest,
    es_final_result,
    es_res_from_mem,
    es_res_from_csr
};

assign es_to_ms_bus = {
    es_pc,
    es_op_ld_b,
    es_op_ld_h,
    es_op_ld_w,
    es_op_st_b,
    es_op_st_h,
    es_op_st_w,
    es_op_ld_bu,
    es_op_ld_hu,
    es_op_ertn,
    es_dest,
    es_gr_we,
    es_res_from_cnt,
    es_res_from_mem,
    es_res_from_csr,
    data_sram_addr[1:0],
    es_mul_div_op,
    es_mul_div_sign,
    es_final_result,
    timer_value,
    es_excp,
    es_excp_num,
    err_addr,
    csr_we,
    csr_num,
    csr_wmask,
    csr_wdata
};

endmodule
