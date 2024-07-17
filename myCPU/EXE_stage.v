`include "myCPU.h"

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
    //forward to ds
    output [`ES_FORWARD_WD   -1:0] es_forward    ,
    // data sram interface
    output                         data_sram_en   ,
    output [                  3:0] data_sram_we,
    output [                 31:0] data_sram_addr ,
    output [                 31:0] data_sram_wdata,
    output [                  1:0] data_sram_size ,
    input                          data_sram_addr_ok
);

/*-------------------- Signal interface --------------------*/
reg  [`DS_TO_ES_BUS_WD -1:0] ds_to_es_bus_r;
wire [18:0] es_alu_op     ;
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
wire        es_inst_load  ;
//DS to ES bus
assign es_pc          = ds_to_es_bus_r[166:135];
assign es_op_ld_b     = ds_to_es_bus_r[134:134];
assign es_op_ld_h     = ds_to_es_bus_r[133:133];
assign es_op_ld_w     = ds_to_es_bus_r[132:132];
assign es_op_st_b     = ds_to_es_bus_r[131:131];
assign es_op_st_h     = ds_to_es_bus_r[130:130];
assign es_op_st_w     = ds_to_es_bus_r[129:129];
assign es_op_ld_bu    = ds_to_es_bus_r[128:128];
assign es_op_ld_hu    = ds_to_es_bus_r[127:127];
assign es_imm         = ds_to_es_bus_r[126: 95];
assign es_rkd_value   = ds_to_es_bus_r[ 94: 63];
assign es_rj_value    = ds_to_es_bus_r[ 62: 31];
assign es_src1_is_pc  = ds_to_es_bus_r[ 30: 30];
assign es_src2_is_imm = ds_to_es_bus_r[ 29: 29];
assign es_src2_is_4   = ds_to_es_bus_r[ 28: 28];
assign es_alu_op      = ds_to_es_bus_r[ 27:  9];
assign es_mem_en      = ds_to_es_bus_r[  8:  8];
assign es_mem_we      = ds_to_es_bus_r[  7:  7];
assign es_dest        = ds_to_es_bus_r[  6:  2];
assign es_gr_we       = ds_to_es_bus_r[  1:  1];
assign es_res_from_mem= ds_to_es_bus_r[  0:  0];
assign es_inst_load   = es_op_ld_b || es_op_ld_h || es_op_ld_w || es_op_ld_bu || es_op_ld_hu;
//ES to MS bus
assign es_to_ms_bus [31:0] = es_pc;
assign es_to_ms_bus [63:32] = es_alu_result;
assign es_to_ms_bus [68:64] = es_dest;
assign es_to_ms_bus [69] = es_gr_we;
assign es_to_ms_bus [70] = es_res_from_mem;
assign es_to_ms_bus [71] = es_op_st_h;
assign es_to_ms_bus [72] = es_op_st_b;
assign es_to_ms_bus [73] = es_op_st_w;
assign es_to_ms_bus [74] = es_op_ld_hu;
assign es_to_ms_bus [75] = es_op_ld_h;
assign es_to_ms_bus [76] = es_op_ld_bu;
assign es_to_ms_bus [77] = es_op_ld_b;
assign es_to_ms_bus [78] = es_op_ld_w;
assign es_to_ms_bus [79] = es_mem_we;

//forward to DS
assign es_forward [0] = es_valid;
assign es_forward [1] = es_gr_we;
assign es_forward [6:2] = es_dest;
assign es_forward [38:7] = es_alu_result;
assign es_forward [70:39] = es_pc;
assign es_forward [71] = es_inst_load;
/* --------------  Handshaking  -------------- */
reg         es_valid      ;
wire        es_ready_go   ;
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

assign es_alu_src1 = es_src1_is_pc  ? es_pc[31:0] : es_rj_value;         
assign es_alu_src2 = es_src2_is_4 ? 32'h0004 : es_src2_is_imm ? es_imm : es_rkd_value;

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

//assign es_addr00 = data_sram_addr[1:0] == 2'b00;
//assign es_addr01 = data_sram_addr[1:0] == 2'b01;
//assign es_addr10 = data_sram_addr[1:0] == 2'b10;
//assign es_addr11 = data_sram_addr[1:0] == 2'b11;
//assign data_sram_wstrb_sp= {4{es_op_st_b && es_addr00}} & 4'b0001 |
//                         {4{es_op_st_b && es_addr01}} & 4'b0010 |
//                         {4{es_op_st_b && es_addr10}} & 4'b0100 |
//                         {4{es_op_st_b && es_addr11}} & 4'b1000 |
//                         {4{es_op_st_h && es_addr00}} & 4'b0011 |
//                         {4{es_op_st_h && es_addr10}} & 4'b1100 |
//                         {4{es_op_st_w}}              & 4'b1111;

//assign data_sram_wstrb = es_mem_we ? data_sram_wstrb_sp : 4'h0;
assign data_sram_wdata = {32{es_op_st_b}} & {4{es_rkd_value[ 7:0]}} |
                         {32{es_op_st_h}} & {2{es_rkd_value[15:0]}} |
                         {32{es_op_st_w}} & es_rkd_value[31:0];
assign data_sram_wr    = es_mem_we;





assign data_sram_size  = {2{es_op_st_b || es_op_ld_b || es_op_ld_bu}} & 2'b00 |
                         {2{es_op_st_h || es_op_ld_h || es_op_ld_hu}} & 2'b01 |
                         {2{es_op_st_w || es_op_ld_w}}                & 2'b10;


assign data_sram_en    = es_mem_en;//存储的使能
assign data_sram_we   = es_mem_we&&es_valid ? 4'hf : 4'h0;
assign data_sram_addr  = es_alu_result;
assign data_sram_wdata = {32{es_op_st_b}} & {4{es_rkd_value[ 7:0]}} |
                         {32{es_op_st_h}} & {2{es_rkd_value[15:0]}} |
                         {32{es_op_st_w}} & es_rkd_value[31:0];

endmodule
