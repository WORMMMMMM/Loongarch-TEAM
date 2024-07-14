`include "mycpu.h"

module exe_stage(
    input                          clk           ,
    input                          reset         ,
    //allowin
    input                          ms_allowin    ,
    output                         es_allowin    ,
    //from ds
    input                          ds_to_es_valid,
    input  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus  ,
    //to ms
    output                         es_to_ms_valid,
    output [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus  ,
    // data sram interface
    output                         data_sram_en   ,
    output [                  3:0] data_sram_wen,
    output [                 31:0] data_sram_addr ,
    output [                 31:0] data_sram_wdata,
    output [                  1:0] data_sram_size ,
    input                          data_sram_addr_ok,
)
reg         es_valid      ;
wire        es_ready_go   ;

reg  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus_r;
wire [18:0] es_alu_op     ;
wire        es_load_op    ;
wire        es_src1_is_sa ;  
wire        es_src1_is_pc ;
wire        es_src2_is_imm; 
wire        es_src2_is_4  ;//前边也要改
wire        es_gr_we      ;
wire        es_mem_we     ;
wire [ 4:0] es_dest       ;
wire [15:0] es_imm        ;
wire [31:0] es_rj_value   ;
wire [31:0] es_rkd_value   ;
wire [31:0] es_pc         ;
wire        es_op_ld_w    ;
wire        es_op_ld_b    ;
wire        es_op_ld_bu   ;
wire        es_op_ld_h    ;
wire        es_op_ld_hu   ;
wire        es_op_st_b    ;
wire        es_op_st_h    ;
wire        es_op_st_w    ;
wire        es_res_from_mem;
/* --------------  Signals interface  -------------- */
assign {es_alu_op      ,  //141:124
        es_op_ld_w     ,  //77
        es_op_ld_b     ,  //76
        es_op_ld_bu    ,  //75
        es_op_ld_h     ,  //74
        es_op_ld_hu    ,  //73
        es_op_st_w     ,  //72
        es_op_st_b     ,  //71
        es_op_st_h     ,  //70
        es_src1_is_pc  ,  //121:121
        es_src2_is_imm ,  //120:120
        es_src2_is_4  ,  //119:119 modified
        es_res_from_mem,  //118:118
        es_gr_we       ,  //118:118
        es_mem_we      ,  //117:117
        es_dest        ,  //116:112
        es_imm         ,  //111:96
        es_rj_value    ,  //95 :64
        es_rkd_value    ,  //63 :32
        es_pc             //31 :0
       } = ds_to_es_bus_r;//接收到的控制信号

assign es_to_ms_bus = {
                       es_op_ld_w     ,  //77
                       es_op_ld_b     ,  //76
                       es_op_ld_bu    ,  //75
                       es_op_ld_h     ,  //74
                       es_op_ld_hu    ,  //73
                       es_op_st_w     ,  //72
                       es_op_st_b     ,  //71
                       es_op_st_h     ,  //70
                       es_res_from_mem,  //69
                       es_mem_we      ,  //70
                       es_gr_we       ,  //69:69
                       es_dest        ,  //68:64
                       es_alu_result  ,  //63:32
                       es_pc             //31:0
                      };//发送的控制信号
/* --------------  Handshaking  -------------- */
assign es_ready_go    = 1'b1;
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
wire [31:0] es_alu_src1   ;
wire [31:0] es_alu_src2   ;
wire [31:0] es_alu_result ;
assign es_alu_src1 = es_src1_is_pc  ? es_pc[31:0] : es_rj_value;         
assign es_alu_src2 = es_src2_is_imm ? es_imm : es_rkd_value;

alu u_alu(
    .clk        (clk          ),
    .reset      (reset        ),
    .alu_op     (es_alu_op    ),
    .alu_src1   (es_alu_src1  ),
    .alu_src2   (es_alu_src2  ),
    .alu_result (es_alu_result),
    .div_ready_go (alu_ready_go)
    );
/* --------------  MEM write interface  -------------- */

assign data_sram_size  = {2{es_op_st_b || es_op_ld_b || es_op_ld_bu}} & 2'b00 |
                         {2{es_op_st_h || es_op_ld_h || es_op_ld_hu}} & 2'b01 |
                         {2{es_op_st_w || es_op_ld_w}}                & 2'b10;


assign data_sram_en    = 1'b1;
assign data_sram_wen   = es_mem_we&&es_valid ? 4'hf : 4'h0;
assign data_sram_addr  = es_alu_result;
assign data_sram_wdata = es_rdk_value;

endmodule
