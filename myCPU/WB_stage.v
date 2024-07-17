`include "myCPU.h"

module wb_stage(
    input                           clk           ,
    input                           reset         ,
    //allowin
    output                          ws_allowin    ,
    //from ms
    input                           ms_to_ws_valid,
    input  [`MS_TO_WS_BUS_WD-1:0]  ms_to_ws_bus  ,
    //to rf: for write back
    output [`WB_BUS_WD-1:0]        ws_to_rf_bus  ,
    //forward to ds
    output [`WS_FORWARD_WD-1:0]    ws_forward    ,
    //trace debug interface
    output [31:0] debug_wb_pc     ,
    output [ 3:0] debug_wb_rf_we ,
    output [ 4:0] debug_wb_rf_wnum,
    output [31:0] debug_wb_rf_wdata
);
/* --------------  Signal interface -------------- */
// MS to WS bus
reg [`MS_TO_WS_BUS_WD -1:0] ms_to_ws_bus_r;
wire        ws_gr_we;
wire [ 4:0] ws_dest;
wire [31:0] ws_final_result;
wire [31:0] ws_pc;
assign ws_pc = ms_to_ws_bus_r[31:0];
assign ws_final_result = ms_to_ws_bus_r[63:32];
assign ws_dest = ms_to_ws_bus_r[68:64];
assign ws_gr_we = ms_to_ws_bus_r[69];

//WS to RF bus
wire        rf_we;
wire [4 :0] rf_waddr;
wire [31:0] rf_wdata;
assign ws_to_rf_bus [31:0] = rf_wdata;
assign ws_to_rf_bus [36:32] = rf_waddr;
assign ws_to_rf_bus [37] = rf_we;

//forward to DS
assign ws_forward [0] = ws_valid;
assign ws_forward [1] = ws_gr_we;
assign ws_forward [6:2] = ws_dest;
assign ws_forward [38:7] = ws_final_result;
assign ws_forward [70:39] = ws_pc;

/*----------------- Handshaking-----------------*/
reg         ws_valid;
wire        ws_ready_go;
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

assign rf_we    = ws_gr_we && ws_valid;
assign rf_waddr = ws_dest;
assign rf_wdata = ws_final_result;

// debug info generate
assign debug_wb_pc       = ws_pc;
assign debug_wb_rf_we   = {4{rf_we}};
assign debug_wb_rf_wnum  = ws_dest;
assign debug_wb_rf_wdata = ws_final_result;

endmodule
