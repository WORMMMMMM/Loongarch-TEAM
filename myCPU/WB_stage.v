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

wire [5:0] csr_ecode;
wire [2:0] csr_esubcode;

/* write back */
wire        rf_we;
wire [4 :0] rf_waddr;
wire [31:0] rf_wdata;


assign ws_ready_go = 1'b1;
assign ws_allowin  = !ws_valid
                   || ws_ready_go;
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

assign ws_pc           = ms_to_ws_bus_r[ 31:  0];
assign ws_ertn         = ms_to_ws_bus_r[ 32: 32];
assign ws_dest         = ms_to_ws_bus_r[ 37: 33];
assign ws_gr_we        = ms_to_ws_bus_r[ 38: 38];
assign ws_res_from_csr = ms_to_ws_bus_r[ 39: 39];
assign ws_final_result = ms_to_ws_bus_r[ 71: 40];
assign ms_excp         = ms_to_ws_bus_r[ 72: 72];
assign ms_excp_num     = ms_to_ws_bus_r[ 88: 73];
assign csr_we          = ms_to_ws_bus_r[ 89: 89];
assign csr_num         = ms_to_ws_bus_r[103: 90];
assign csr_wmask       = ms_to_ws_bus_r[135:104];
assign csr_wdata       = ms_to_ws_bus_r[167:136];

// forward to DS
assign ws_forward [0] = ws_valid;
assign ws_forward [1] = ws_gr_we;
assign ws_forward [6:2] = ws_dest;
assign ws_forward [38:7] = ws_final_result;
assign ws_forward [70:39] = ws_pc;

// debug info generate
assign debug_wb_pc       = ws_pc;
assign debug_wb_rf_we    = {4{rf_we}};
assign debug_wb_rf_wnum  = ws_dest;
assign debug_wb_rf_wdata = ws_final_result;

assign ws_excp     = ms_excp;
assign ws_excp_num = ms_excp_num;

assign excp_flush = ws_valid & ws_excp;
assign ertn_flush = ws_valid & ws_ertn;


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

assign rf_we    = ws_gr_we && ws_valid && ~ws_excp;
assign rf_waddr = ws_dest;
assign rf_wdata = ws_res_from_csr ? csr_rdata : ws_final_result;

assign wb_bus[31: 0] = rf_wdata;
assign wb_bus[36:32] = rf_waddr;
assign wb_bus[37:37] = rf_we;

endmodule
