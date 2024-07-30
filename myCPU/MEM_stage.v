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
    input  data_sram_data_ok,
    
    input  excp_flush,
    input  ertn_flush,
    output ms_ex
);

/* handshaking */
wire ms_flush;
wire ms_stall;
wire ms_ready_go;
reg  ms_valid;
reg [`ES_TO_MS_BUS_WD-1:0] es_to_ms_bus_r;

wire [31:0] ms_pc;
wire        ms_op_ld_b;
wire        ms_op_ld_h;
wire        ms_op_ld_w;
wire        ms_op_st_b;
wire        ms_op_st_h;
wire        ms_op_st_w;
wire        ms_op_ld_bu;
wire        ms_op_ld_hu;
wire        ms_op_ertn;
wire [ 4:0] ms_dest;
wire        ms_gr_we;
wire        ms_res_from_cnt;
wire        ms_res_from_mem;
wire        ms_res_from_csr;
wire [ 1:0] ms_addr_lowbits;
wire [ 3:0] ms_mul_div_op;
wire        ms_mul_div_sign;
wire [31:0] es_final_result;
wire [31:0] timer_value;

wire [31:0] mem_result;
wire [31:0] ms_final_result;

wire [ 7:0] mem_byte_data;
wire [15:0] mem_halfword_data;

// SRAM data buffer
reg  [32:0] data_sram_rdata_buf;
reg         data_sram_rdata_buf_valid;
wire [32:0] final_data_sram_rdata;

/* exception */
wire es_excp;
wire ms_excp;
wire [15:0] es_excp_num;
wire [15:0] ms_excp_num;
wire [31:0] err_addr;

/* csr */
wire csr_we;
wire [13:0] csr_num;
wire [31:0] csr_wmask;
wire [31:0] csr_wdata;


assign ms_flush       = excp_flush || ertn_flush;
assign ms_stall       = ms_res_from_mem && !data_sram_data_ok && !data_sram_rdata_buf_valid;
assign ms_ready_go    = !ms_flush && !ms_stall;
assign ms_allowin     = !ms_valid || ms_ready_go && ws_allowin || ms_flush;
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

assign {
    ms_pc,
    ms_op_ld_b,
    ms_op_ld_h,
    ms_op_ld_w,
    ms_op_st_b,
    ms_op_st_h,
    ms_op_st_w,
    ms_op_ld_bu,
    ms_op_ld_hu,
    ms_op_ertn,
    ms_dest,
    ms_gr_we,
    ms_res_from_cnt,
    ms_res_from_mem,
    ms_res_from_csr,
    ms_addr_lowbits,
    ms_mul_div_op,
    ms_mul_div_sign,
    es_final_result,
    timer_value,
    es_excp,
    es_excp_num,
    err_addr,
    csr_we,
    csr_num,
    csr_wmask,
    csr_wdata
} = es_to_ms_bus_r;            

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
assign mem_halfword_data = {16{addr00}} & final_data_sram_rdata[15: 0] |
                           {16{addr10}} & final_data_sram_rdata[31:16];
// mem_result mux
assign mem_result = {32{ms_op_ld_w}}  & final_data_sram_rdata                            |
                    {32{ms_op_ld_b}}  & {{24{mem_byte_data[7]}}, mem_byte_data}          |
                    {32{ms_op_ld_bu}} & {24'b0, mem_byte_data}                           |
                    {32{ms_op_ld_h}}  & {{16{mem_halfword_data[15]}}, mem_halfword_data} |
                    {32{ms_op_ld_hu}} & {16'b0, mem_halfword_data};

assign ms_final_result = ({32{ms_res_from_cnt }} & timer_value      ) |
                         ({32{ms_res_from_mem }} & mem_result       ) |
                         ({32{ms_mul_div_op[0]}} & mul_result[31:0] ) |
                         ({32{ms_mul_div_op[1]}} & mul_result[63:32]) |
                         ({32{ms_mul_div_op[2]}} & div_result       ) |
                         ({32{ms_mul_div_op[3]}} & mod_result       ) |
                         ({32{!ms_mul_div_sign && !ms_res_from_mem}} & es_final_result);

assign ms_excp     = es_excp;
assign ms_excp_num = es_excp_num;

assign ms_ex = ms_valid && (ms_excp || ms_op_ertn);

assign ms_forward = {
    ms_valid,
    ms_gr_we,
    ms_dest,
    ms_final_result,
    ms_res_from_mem,
    ms_res_from_csr
};

assign ms_to_ws_bus = {
    ms_pc,
    ms_op_ertn,
    ms_dest,
    ms_gr_we,
    ms_res_from_csr,
    ms_final_result,
    ms_excp,
    ms_excp_num,
    err_addr,
    csr_we,
    csr_num,
    csr_wmask,
    csr_wdata
};

endmodule
