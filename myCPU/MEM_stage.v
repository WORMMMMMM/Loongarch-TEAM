`include "myCPU.h"

module mem_stage(
    input                          clk           ,
    input                          reset         ,
    //allowin
    input                          ws_allowin    ,
    output                         ms_allowin    ,
    //from es
    input                          es_to_ms_valid,
    input  [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus  ,
    //to ws
    output                         ms_to_ws_valid,
    output [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus  ,
    //forward to ds
    output [`MS_FORWARD_WD   -1:0] ms_forward    ,
    //from data-sram
    input  [31                 :0] data_sram_rdata
);

/* --------------  Signal interface  -------------- */
//ES to MS bus
reg [`ES_TO_MS_BUS_WD -1:0] es_to_ms_bus_r;
wire        ms_res_from_mem;
wire        ms_gr_we;
wire [ 4:0] ms_dest;
wire [31:0] ms_alu_result;
wire [31:0] ms_pc;

assign ms_pc = es_to_ms_bus_r[31:0];
assign ms_alu_result = es_to_ms_bus_r[63:32];
assign ms_dest = es_to_ms_bus_r[68:64];
assign ms_gr_we = es_to_ms_bus_r[69];
assign ms_res_from_mem = es_to_ms_bus_r[70];
assign ms_op_st_h = es_to_ms_bus_r[71];
assign ms_op_st_b = es_to_ms_bus_r[72];
assign ms_op_st_w = es_to_ms_bus_r[73];
assign ms_op_ld_hu = es_to_ms_bus_r[74];
assign ms_op_ld_h = es_to_ms_bus_r[75];
assign ms_op_ld_bu = es_to_ms_bus_r[76];
assign ms_op_ld_b = es_to_ms_bus_r[77];
assign ms_op_ld_w = es_to_ms_bus_r[78];
assign ms_mem_we = es_to_ms_bus_r[79];

//MS to WS bus
wire [31:0] mem_result;
wire [31:0] ms_final_result;

assign ms_to_ws_bus [31:0] = ms_pc;
assign ms_to_ws_bus [63:32] = ms_final_result;
assign ms_to_ws_bus [68:64] = ms_dest;
assign ms_to_ws_bus [69] = ms_gr_we;

//forward to DS
assign ms_forward [0] = ms_valid;
assign ms_forward [1] = ms_gr_we;
assign ms_forward [6:2] = ms_dest;
assign ms_forward [38:7] = ms_final_result;
assign ms_forward [39] = ms_res_from_mem;
assign ms_forward [71:40] = ms_pc;
/*----------------- Handshaking-----------------*/                      
reg         ms_valid;
wire        ms_ready_go;
assign ms_ready_go    = 1'b1;
assign ms_allowin     = !ms_valid || ms_ready_go && ws_allowin;
assign ms_to_ws_valid = ms_valid && ms_ready_go;
always @(posedge clk) begin
    if (reset) begin
        ms_valid <= 1'b0;
    end
    else if (ms_allowin) begin
        ms_valid <= es_to_ms_valid;
    end

    if (es_to_ms_valid && ms_allowin) begin
        es_to_ms_bus_r  <= es_to_ms_bus;
    end
end

assign mem_result = data_sram_rdata;

assign ms_final_result = ms_res_from_mem ? mem_result
                                         : ms_alu_result;

endmodule
