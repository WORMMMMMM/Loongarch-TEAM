`include "myCPU.h"

module wb_stage(
    input  clk,
    input  reset,

    // allowin
    output ws_allowin,

    // from ms
    input  ms_to_ws_valid,
    input  [`MS_TO_WS_BUS_WD-1:0] ms_to_ws_bus,

    output [`WB_BUS_WD-1:0] wb_bus,

    // forward to ds
    output [`WS_FORWARD_WD-1:0] ws_forward,
    
    output ws_ex,
    output excp_flush,
    output ertn_flush,
    output [31:0] era,
    output [31:0] eentry,
    
    input  [ 7:0] hard_int_in,
    input         ipi_int_in,
    output has_int,

    // trace debug interface
    output [31:0] debug_wb_pc,
    output [ 3:0] debug_wb_rf_we,
    output [ 4:0] debug_wb_rf_wnum,
    output [31:0] debug_wb_rf_wdata
);

/* handshaking */
wire ws_ready_go;
reg  ws_valid;
reg [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus_r;

wire [31:0] ws_pc;
wire ws_ertn;
wire [ 4:0] ws_dest;
wire ws_gr_we;
wire ws_res_from_csr;
wire [31:0] ms_final_result;
wire [31:0] ws_final_result;

/* exception */
wire ms_excp;
wire ws_excp;
wire [15:0] ms_excp_num;
wire [15:0] ws_excp_num;

/* csr */
wire csr_we;
wire [13:0] csr_num;
wire [31:0] csr_wmask;
wire [31:0] csr_wdata;
wire [31:0] csr_rdata;

wire [5:0] csr_ecode;
wire [2:0] csr_esubcode;

/* write back */
wire        rf_we;
wire [4 :0] rf_waddr;
wire [31:0] rf_wdata;


assign ws_ready_go = 1'b1;
assign ws_allowin  = !ws_valid || ws_ready_go;
always @(posedge clk) begin
    if (reset) begin
        ws_valid <= 1'b0;
    end
    else if (ws_allowin) begin
        ws_valid <= ms_to_ws_valid;
    end

    if (ms_to_ws_valid && ws_allowin) begin
        ms_to_ws_bus_r <= ms_to_ws_bus;
    end
end

assign {
    ws_pc,
    ws_ertn,
    ws_dest,
    ws_gr_we,
    ws_res_from_csr,
    ms_final_result,
    ms_excp,
    ms_excp_num,
    csr_we,
    csr_num,
    csr_wmask,
    csr_wdata
} = ms_to_ws_bus_r;

// debug info generate
assign debug_wb_pc       = ws_pc;
assign debug_wb_rf_we    = {4{rf_we}};
assign debug_wb_rf_wnum  = rf_waddr;
assign debug_wb_rf_wdata = rf_wdata;

assign ws_excp     = ms_excp;
assign ws_excp_num = ms_excp_num;

// assign ws_ex      = ws_valid && ws_excp || ws_excp_num[15] !== 1'bx && ws_excp_num[15];
// assign excp_flush = ws_valid && ws_excp || ws_excp_num[15] !== 1'bx && ws_excp_num[15];
assign ws_ex      = ws_valid && ws_excp;
assign excp_flush = ws_valid && ws_excp;
assign ertn_flush = ws_valid && ws_ertn;

assign csr_ecode = ws_excp_num[15] ? `ECODE_INT :
                   ws_excp_num[14] ? `ECODE_ADE : 
                   ws_excp_num[13] ? `ECODE_TLBR :
                   ws_excp_num[12] ? `ECODE_PIF :
                   ws_excp_num[11] ? `ECODE_PPI :
                   ws_excp_num[10] ? `ECODE_SYS :
                   ws_excp_num[ 9] ? `ECODE_BRK :
                   ws_excp_num[ 8] ? `ECODE_INE :
                   ws_excp_num[ 7] ? `ECODE_IPE :
                   ws_excp_num[ 6] ? `ECODE_ALE :
                   ws_excp_num[ 5] ? `ECODE_TLBR :
                   ws_excp_num[ 4] ? `ECODE_PME :
                   ws_excp_num[ 3] ? `ECODE_PPI :
                   ws_excp_num[ 2] ? `ECODE_PIS :
                   ws_excp_num[ 1] ? `ECODE_PIL :
                   6'b0;

assign csr_esubcode = 3'b0;

regcsr u_regcsr(
    .clk         (clk         ),
    .reset       (reset       ),

    .csr_we      (csr_we      ),
    .csr_num     (csr_num     ),
    .csr_wmask   (csr_wmask   ),
    .csr_wdata   (csr_wdata   ),
    .csr_rdata   (csr_rdata   ),

    .excp_flush  (excp_flush  ),
    .ertn_flush  (ertn_flush  ),
    .ecode       (csr_ecode   ),
    .esubcode    (csr_esubcode),
    .epc         (ws_pc       ),

    .era         (era         ),
    .eentry      (eentry      ),

    .hard_int_in (hard_int_in ),
    .ipi_int_in  (ipi_int_in  ),
    .has_int     (has_int     )
);

assign ws_final_result = ws_res_from_csr ? csr_rdata : ms_final_result;

assign rf_we    = ws_gr_we && ws_valid && ~ws_excp;
assign rf_waddr = ws_dest;
assign rf_wdata = ws_final_result;

assign wb_bus = {
    rf_we,
    rf_waddr,
    rf_wdata
};

assign ws_forward = {
    ws_valid,
    ws_gr_we,
    ws_dest,
    ws_final_result
};

endmodule
