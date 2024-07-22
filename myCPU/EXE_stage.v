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
    output        data_sram_req,
    output [ 3:0] data_sram_wstrb,
    output [ 1:0] data_sram_size,
    output        data_sram_en,
    output [ 3:0] data_sram_we,
    output [31:0] data_sram_addr,
    output [31:0] data_sram_wdata,

    input  excp_flush,
    input  ertn_flush
);

/*-------------------- Signal interface --------------------*/
reg  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus_r;
wire [18:0] es_alu_op     ;
wire [ 3:0] es_mul_div_op ;
wire        es_load_op    ;
wire        es_src1_is_sa ;
wire        es_src1_is_pc ;
wire        es_src2_is_imm;
wire        es_src2_is_4  ;
wire        es_gr_we      ;
wire        es_mem_we     ;
wire [ 4:0] es_dest       ;
wire [31:0] es_imm        ;
wire [31:0] es_rj_value   ;
wire [31:0] es_rkd_value  ;
wire [31:0] es_pc         ;
wire [ 3:0] es_mul_div_op ; 
wire        es_op_ld_w    ;
wire        es_op_ld_b    ;
wire        es_op_ld_bu   ;
wire        es_op_ld_h    ;
wire        es_op_ld_hu   ;
wire        es_op_st_b    ;
wire        es_op_st_h    ;
wire        es_op_st_w    ;
wire        es_res_from_mem;
wire        er_res_from_csr;

/* --------------  Signals interface  -------------- */
wire        es_inst_load  ;
wire [31:0] es_alu_result ;

//DS to ES bus
assign es_pc           = ds_to_es_bus_r[ 31:  0];
assign es_op_ld_b      = ds_to_es_bus_r[ 32: 32];
assign es_op_ld_h      = ds_to_es_bus_r[ 33: 33];
assign es_op_ld_w      = ds_to_es_bus_r[ 34: 34];
assign es_op_st_b      = ds_to_es_bus_r[ 35: 35];
assign es_op_st_h      = ds_to_es_bus_r[ 36: 36];
assign es_op_st_w      = ds_to_es_bus_r[ 37: 37];
assign es_op_ld_bu     = ds_to_es_bus_r[ 38: 38];
assign es_op_ld_hu     = ds_to_es_bus_r[ 39: 39];
assign es_imm          = ds_to_es_bus_r[ 71: 40];
assign es_rkd_value    = ds_to_es_bus_r[103: 72];
assign es_rj_value     = ds_to_es_bus_r[135:104];
assign es_src1_is_pc   = ds_to_es_bus_r[136:136];
assign es_src2_is_imm  = ds_to_es_bus_r[137:137];
assign es_src2_is_4    = ds_to_es_bus_r[138:138];
assign es_alu_op       = ds_to_es_bus_r[157:139];
assign es_mem_en       = ds_to_es_bus_r[158:158];
assign es_mem_we       = ds_to_es_bus_r[159:159];
assign es_dest         = ds_to_es_bus_r[164:160];
assign es_gr_we        = ds_to_es_bus_r[165:165];
assign es_res_from_mem = ds_to_es_bus_r[166:166];
assign es_res_from_csr = ds_to_es_bus_r[167:167];
assign es_mul_div_op   = ds_to_es_bus_r[171:168];
assign es_mul_div_sign = ds_to_es_bus_r[172:172];
assign es_inst_load    = es_op_ld_b || es_op_ld_h || es_op_ld_w || es_op_ld_bu || es_op_ld_hu;

//forward to DS
assign es_forward [0] = es_valid;
assign es_forward [1] = es_gr_we;
assign es_forward [6:2] = es_dest;
assign es_forward [38:7] = es_alu_result;
assign es_forward [70:39] = es_pc;
assign es_forward [71] = es_inst_load;
/* --------------  Handshaking  -------------- */
reg  es_valid;
wire es_ready_go;

assign es_ready_go    = !es_div_stall;
assign es_allowin     = !es_valid || es_ready_go && ms_allowin;
assign es_to_ms_valid =  es_valid && es_ready_go;
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

/* --------------  ALU interface  -------------- */
wire [31:0] es_alu_src1;
wire [31:0] es_alu_src2;
wire        es_div_enable;
wire        es_mul_enable;
wire        es_div_stall;

assign es_alu_src1 = es_src1_is_pc  ? es_pc[31:0] :
                     es_rj_value;         
assign es_alu_src2 = es_src2_is_4   ? 32'h0004 :
                     es_src2_is_imm ? es_imm :
                     es_rkd_value;
assign es_div_enable = (es_mul_div_op[2] | es_mul_div_op[3]) & es_valid;
assign es_mul_enable = (es_mul_div_op[0] | es_mul_div_op[1]) & es_valid;
assign es_div_stall  = es_div_enable & ~div_complete;

alu u_alu(
    .alu_op       (es_alu_op    ),
    .alu_src1     (es_alu_src1  ),
    .alu_src2     (es_alu_src2  ),
    .alu_result   (es_alu_result),
    .div_ready_go (alu_ready_go )
    );

/* --------------  MEM write interface  -------------- */

wire [ 3:0] data_sram_wstrb_sp;
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

assign data_sram_wstrb = es_mem_we ? data_sram_wstrb_sp : 4'h0;
assign data_sram_wdata = {32{es_op_st_b}} & {4{es_rkd_value[ 7:0]}} |
                         {32{es_op_st_h}} & {2{es_rkd_value[15:0]}} |
                         {32{es_op_st_w}} & es_rkd_value[31:0];
assign data_sram_wr    = |data_sram_wstrb;

assign data_sram_size  = {2{es_op_st_b || es_op_ld_b || es_op_ld_bu}} & 2'b00 |
                         {2{es_op_st_h || es_op_ld_h || es_op_ld_hu}} & 2'b01 |
                         {2{es_op_st_w || es_op_ld_w}}                & 2'b10;


assign data_sram_en    = es_mem_en;
assign data_sram_we    = es_mem_we && es_valid ? 4'hf : 4'h0;
assign data_sram_addr  = es_alu_result;

assign data_sram_req    = (es_res_from_mem || es_mem_we) && es_valid 
                      && ms_allowin;


/* exception */
wire flush;

wire excp_ale;

wire ds_excp;
wire [15:0] ds_excp_num;
wire es_excp;
wire [15:0] ex_excp_num;

/* csr */
wire csr_we;
wire [13:0] csr_num;
wire [31:0] csr_wmask;
wire [31:0] csr_wdata;


assign flush = excp_flush | ertn_flush;

assign excp_ale = (es_op_ld_h | es_op_st_h | es_op_ld_hu) & data_sram_addr[0] != 1'b0
                | (es_op_ld_w | es_op_st_w) & data_sram_addr[1:0] != 2'b00;

assign ds_excp     = ds_to_es_bus_r[173:173];
assign ds_excp_num = ds_to_es_bus_r[189:174];
assign es_excp     = ds_excp | excp_ale;
assign ex_excp_num = ds_excp_num | {9'b0, excp_ale, 6'b0};

assign csr_we    = ds_to_es_bus_r[190:190];
assign csr_num   = ds_to_es_bus_r[204:191];
assign csr_wmask = ds_to_es_bus_r[236:205];
assign csr_wdata = ds_to_es_bus_r[268:237];

assign es_to_ms_bus[ 31:  0] = es_pc;
assign es_to_ms_bus[ 63: 32] = es_alu_result;
assign es_to_ms_bus[ 68: 64] = es_dest;
assign es_to_ms_bus[ 69: 69] = es_gr_we;
assign es_to_ms_bus[ 70: 70] = es_res_from_mem;
assign es_to_ms_bus[ 71: 71] = es_res_from_csr;
assign es_to_ms_bus[ 72: 72] = es_op_st_h;
assign es_to_ms_bus[ 73: 73] = es_op_st_b;
assign es_to_ms_bus[ 74: 74] = es_op_st_w;
assign es_to_ms_bus[ 75: 75] = es_op_ld_hu;
assign es_to_ms_bus[ 76: 76] = es_op_ld_h;
assign es_to_ms_bus[ 77: 77] = es_op_ld_bu;
assign es_to_ms_bus[ 78: 78] = es_op_ld_b;
assign es_to_ms_bus[ 79: 79] = es_op_ld_w;
assign es_to_ms_bus[ 80: 80] = es_mem_we;
assign es_to_ms_bus[ 82: 81] = data_sram_addr[1:0];
assign es_to_ms_bus[ 86: 83] = es_mul_div_op;
assign es_to_ms_bus[ 87: 87] = es_mul_div_sign;
assign es_to_ms_bus[ 88: 88] = es_excp;
assign es_to_ms_bus[104: 89] = ex_excp_num;
assign es_to_ms_bus[105:105] = csr_we;
assign es_to_ms_bus[119:106] = csr_num;
assign es_to_ms_bus[151:120] = csr_wmask;
assign es_to_ms_bus[183:152] = csr_wdata;

endmodule
