`include "myCPU.h"

module mem_stage(
    input  clk,
    input  reset,

    // allowin
    input  ws_allowin,
    output ms_allowin,

    // from es
    input  es_to_ms_valid,
    input  [`ES_TO_MS_BUS_WD-1:0] es_to_ms_bus,

    // to ws
    output ms_to_ws_valid,
    output [`MS_TO_WS_BUS_WD-1:0] ms_to_ws_bus,

    // forward to ds
    output [`MS_FORWARD_WD-1:0] ms_forward,

    // div    
    input  [31:0] div_result,
    input  [31:0] mod_result,
    input  [63:0] mul_result,

    // from data-sram
    input  [31:0] data_sram_rdata,
    
    input  excp_flush,
    input  ertn_flush
);

/* --------------  Signal interface  -------------- */
//ES to MS bus
reg [`ES_TO_MS_BUS_WD-1:0] es_to_ms_bus_r;
wire        ms_res_from_mem;
wire        ms_res_from_csr;
wire        ms_gr_we;
wire [ 4:0] ms_dest;
wire [31:0] ms_alu_result;
wire [31:0] ms_pc;
wire [ 1:0] ms_addr_lowbits;
wire        ms_mul_div_sign;
wire [ 3:0] ms_mul_div_op;

assign ms_pc           = es_to_ms_bus_r[31: 0];
assign ms_alu_result   = es_to_ms_bus_r[63:32];
assign ms_dest         = es_to_ms_bus_r[68:64];
assign ms_gr_we        = es_to_ms_bus_r[69:69];
assign ms_res_from_mem = es_to_ms_bus_r[70:70];
assign ms_res_from_csr = es_to_ms_bus_r[71:71];
assign ms_op_st_h      = es_to_ms_bus_r[72:72];
assign ms_op_st_b      = es_to_ms_bus_r[73:73];
assign ms_op_st_w      = es_to_ms_bus_r[74:74];
assign ms_op_ld_hu     = es_to_ms_bus_r[75:75];
assign ms_op_ld_h      = es_to_ms_bus_r[76:76];
assign ms_op_ld_bu     = es_to_ms_bus_r[77:77];
assign ms_op_ld_b      = es_to_ms_bus_r[78:78];
assign ms_op_ld_w      = es_to_ms_bus_r[79:79];
assign ms_mem_we       = es_to_ms_bus_r[80:80];
assign ms_addr_lowbits = es_to_ms_bus_r[82:81];
assign ms_mul_div_sign = es_to_ms_bus_r[83:83];
assign ms_mul_div_op   = es_to_ms_bus_r[87:84];

//MS to WS bus
wire [31:0] mem_result;
wire [31:0] ms_final_result;

reg         ms_valid;
wire        ms_ready_go;
//forward to DS
assign ms_forward [0] = ms_valid;
assign ms_forward [1] = ms_gr_we;
assign ms_forward [6:2] = ms_dest;
assign ms_forward [38:7] = ms_final_result;
assign ms_forward [39] = ms_res_from_mem;
assign ms_forward [71:40] = ms_pc;
/*----------------- Handshaking-----------------*/                      

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

/* --------------  MEM read interface  -------------- */
wire [7:0]  mem_byte_data;
wire [15:0] mem_halfword_data;

// SRAM data buffer
reg  [32:0] data_sram_rdata_buf;
reg        data_sram_rdata_buf_valid;
wire [32:0] final_data_sram_rdata;

wire data_sram_data_ok = 1'b1;

always @(posedge clk) begin
    if (reset)
        data_sram_rdata_buf <= 32'b0;
    else if (data_sram_data_ok && ~ws_allowin)      // If data is back, WB stage do not allow in
                                                    // Then write it into buffer, wait for allowin to rise
        data_sram_rdata_buf <= data_sram_rdata;
end

always @(posedge clk) begin
    if (reset)
        data_sram_rdata_buf_valid <= 1'b0;
    else if (data_sram_data_ok && ~ws_allowin)
        data_sram_rdata_buf_valid <= 1'b1;
    else if (ms_ready_go && ws_allowin)
        data_sram_rdata_buf_valid <= 1'b0;
end

assign final_data_sram_rdata = data_sram_rdata_buf_valid ? data_sram_rdata_buf : data_sram_rdata;

// mem_byte_data mux 
assign addr00 = ms_addr_lowbits == 2'b00;
assign addr01 = ms_addr_lowbits == 2'b01;
assign addr10 = ms_addr_lowbits == 2'b10;
assign addr11 = ms_addr_lowbits == 2'b11;
assign mem_byte_data = {8{addr00}} & final_data_sram_rdata[7:0]   |
                       {8{addr01}} & final_data_sram_rdata[15:8]  |
                       {8{addr10}} & final_data_sram_rdata[23:16] |
                       {8{addr11}} & final_data_sram_rdata[31:24];

// mem_halfword_data mux
assign mem_halfword_data = {16{addr00}} & final_data_sram_rdata[15:0] |
                           {16{addr10}} & final_data_sram_rdata[31:16];
// mem_result mux
assign mem_result = {32{ms_op_ld_w}}  & final_data_sram_rdata                                 |
                    {32{ms_op_ld_b}}  & {{24{mem_byte_data[7]}}, mem_byte_data}         |
                    {32{ms_op_ld_bu}} & {24'b0, mem_byte_data}                          |
                    {32{ms_op_ld_h}}  & {{16{mem_halfword_data[15]}}, mem_halfword_data}|
                    {32{ms_op_ld_hu}} & {16'b0, mem_halfword_data};

assign ms_final_result = ({32{ms_res_from_mem }} & mem_result       )  |
                         ({32{ms_mul_div_op[0]}} & mul_result[31:0] )  |
                         ({32{ms_mul_div_op[1]}} & mul_result[63:32])  |
                         ({32{ms_mul_div_op[2]}} & div_result       )  |
                         ({32{ms_mul_div_op[3]}} & mod_result       )  |
                         ({32{!ms_mul_div_sign && !ms_res_from_mem}} & ms_alu_result);

/* exception */
wire flush;

wire es_excp;
wire [15:0] es_excp_num;
wire ms_excp;
wire [15:0] ms_excp_num;

/* csr */
wire csr_we;
wire [13:0] csr_num;
wire [31:0] csr_wmask;
wire [31:0] csr_wdata;

assign flush = excp_flush | ertn_flush;

assign es_excp     = es_to_ms_bus_r[ 88: 88];
assign es_excp_num = es_to_ms_bus_r[104: 89];
assign ms_excp     = es_excp;
assign ms_excp_num = es_excp_num;

assign csr_we    = es_to_ms_bus_r[105:105];
assign csr_num   = es_to_ms_bus_r[119:106];
assign csr_wmask = es_to_ms_bus_r[151:120];
assign csr_wdata = es_to_ms_bus_r[183:152];

assign ms_to_ws_bus[ 31:  0] = ms_pc;
assign ms_to_ws_bus[ 63: 32] = ms_final_result;
assign ms_to_ws_bus[ 68: 64] = ms_dest;
assign ms_to_ws_bus[ 69: 69] = ms_gr_we;
assign ms_to_ws_bus[ 70: 70] = ms_res_from_csr;
assign ms_to_ws_bus[ 71: 71] = ms_excp;
assign ms_to_ws_bus[ 87: 72] = ms_excp_num;
assign ms_to_ws_bus[ 88: 88] = csr_we;
assign ms_to_ws_bus[102: 89] = csr_num;
assign ms_to_ws_bus[134:103] = csr_wmask;
assign ms_to_ws_bus[166:135] = csr_wdata;

endmodule
