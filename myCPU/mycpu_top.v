
`include "myCPU.h"

module mycpu_top(
    input         clk,
    input         resetn,

    // inst sram interface
    output        inst_sram_en,
    output [ 3:0] inst_sram_we,
    output [31:0] inst_sram_addr,
    output [31:0] inst_sram_wdata,
    input  [31:0] inst_sram_rdata,

    // data sram interface
    output        data_sram_en,
    output [ 3:0] data_sram_we,
    output [31:0] data_sram_addr,
    output [31:0] data_sram_wdata,
    input  [31:0] data_sram_rdata,
    
    // trace debug interface
    output [31:0] debug_wb_pc,
    output [ 3:0] debug_wb_rf_we,
    output [ 4:0] debug_wb_rf_wnum,
    output [31:0] debug_wb_rf_wdata
);

reg reset;
always @(posedge clk) reset <= ~resetn;

wire ds_allowin;
wire es_allowin;
wire ms_allowin;
wire ws_allowin;
wire fs_to_ds_valid;
wire ds_to_es_valid;
wire es_to_ms_valid;
wire ms_to_ws_valid;
wire [`FS_TO_DS_BUS_WD-1:0] fs_to_ds_bus;
wire [`DS_TO_ES_BUS_WD-1:0] ds_to_es_bus;
wire [`ES_TO_MS_BUS_WD-1:0] es_to_ms_bus;
wire [`MS_TO_WS_BUS_WD-1:0] ms_to_ws_bus;
wire [`BR_BUS_WD-1:0] br_bus;
wire [`WB_BUS_WD-1:0] wb_bus;
wire [`ES_FORWARD_WD   -1:0] es_forward;
wire [`MS_FORWARD_WD   -1:0] ms_forward;
wire [`WS_FORWARD_WD   -1:0] ws_forward;

if_stage if_stage(
    .clk            (clk            ),
    .reset          (reset          ),

    // allowin
    .ds_allowin     (ds_allowin     ),

    // brbus
    .br_bus         (br_bus         ),

    // to ds
    .fs_to_ds_valid (fs_to_ds_valid ),
    .fs_to_ds_bus   (fs_to_ds_bus   ),

    // inst sram interface
    .inst_sram_en   (inst_sram_en   ),
    .inst_sram_we  (inst_sram_we  ),
    .inst_sram_addr (inst_sram_addr ),
    .inst_sram_wdata(inst_sram_wdata),
    .inst_sram_rdata(inst_sram_rdata)
);

id_stage id_stage(
    .clk            (clk            ),
    .reset          (reset          ),

    // allowin
    .es_allowin     (es_allowin     ),
    .ds_allowin     (ds_allowin     ),
    
    // from fs
    .fs_to_ds_valid (fs_to_ds_valid ),
    .fs_to_ds_bus   (fs_to_ds_bus   ),

    // to es
    .ds_to_es_valid (ds_to_es_valid ),
    .ds_to_es_bus   (ds_to_es_bus   ),

    .br_bus         (br_bus         ),
    .wb_bus         (wb_bus         ),
    .es_forward     (es_forward     ),
    .ms_forward     (ms_forward     ),
    .ws_forward     (ws_forward     )
);

exe_stage exe_stage(
    .clk            (clk            ),
    .reset          (reset          ),

    // allowin
    .ms_allowin     (ms_allowin     ),
    .es_allowin     (es_allowin     ),

    // from ds
    .ds_to_es_valid (ds_to_es_valid ),
    .ds_to_es_bus   (ds_to_es_bus   ),

    // to ms
    .es_to_ms_valid (es_to_ms_valid ),
    .es_to_ms_bus   (es_to_ms_bus   ),
    
    .es_forward     (es_forward     ),

    // data sram interface
    .data_sram_en   (data_sram_en   ),
    .data_sram_we  (data_sram_we  ),
    .data_sram_addr (data_sram_addr ),
    .data_sram_wdata(data_sram_wdata)
);

mem_stage mem_stage(
    .clk            (clk            ),
    .reset          (reset          ),

    // allowin
    .ws_allowin     (ws_allowin     ),
    .ms_allowin     (ms_allowin     ),

    // from es
    .es_to_ms_valid (es_to_ms_valid ),
    .es_to_ms_bus   (es_to_ms_bus   ),

    // to ws
    .ms_to_ws_valid (ms_to_ws_valid ),
    .ms_to_ws_bus   (ms_to_ws_bus   ),
    
    .ms_forward     (ms_forward     ),

    // from data-sram
    .data_sram_rdata(data_sram_rdata)
);

wb_stage wb_stage(
    .clk            (clk            ),
    .reset          (reset          ),

    // allowin
    .ws_allowin     (ws_allowin     ),

    // from ms
    .ms_to_ws_valid (ms_to_ws_valid ),
    .ms_to_ws_bus   (ms_to_ws_bus   ),

    .ws_to_rf_bus   (wb_bus         ),
    
    .ws_forward     (ws_forward     ),

    // trace debug interface
    .debug_wb_pc      (debug_wb_pc      ),
    .debug_wb_rf_we  (debug_wb_rf_we  ),
    .debug_wb_rf_wnum (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata(debug_wb_rf_wdata)
);

endmodule
