
`include "myCPU.h"

module cpu_core(
    input         clk,
    input         resetn,
    
    input  [ 7:0] hard_int_in,

    // inst sram interface
    output        inst_sram_req,
    output [ 3:0] inst_sram_wstrb,
    output [31:0] inst_sram_addr,
    output [31:0] inst_sram_wdata,
    input  [31:0] inst_sram_rdata,
    output [ 1:0] inst_sram_size,
    input         inst_sram_addr_ok,
    input         inst_sram_data_ok,
    output        inst_sram_wr,
    
    // data sram interface
    output        data_sram_req,
    output [ 3:0] data_sram_wstrb,
    output [31:0] data_sram_addr,
    output [31:0] data_sram_wdata,
    input  [31:0] data_sram_rdata,
    output [ 1:0] data_sram_size,
    input         data_sram_addr_ok,
    input         data_sram_data_ok,
    output        data_sram_wr,
    
    // trace debug interface
    output [31:0] debug_wb_pc,
    output [ 3:0] debug_wb_rf_we,
    output [ 4:0] debug_wb_rf_wnum,
    output [31:0] debug_wb_rf_wdata,
    output [31:0] debug_wb_inst 
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

wire [`ES_FORWARD_WD-1:0] es_forward;
wire [`MS_FORWARD_WD-1:0] ms_forward;
wire [`WS_FORWARD_WD-1:0] ws_forward;

wire es_div_enable;
wire es_mul_div_sign;
wire [31:0] es_rj_value;
wire [31:0] es_rkd_value;
wire div_complete;
wire [31:0] div_result;
wire [31:0] mod_result;
wire [63:0] mul_result;
wire [63:0] mul_result_sign;
wire [63:0] mul_result_unsign;

wire ms_ex;
wire ws_ex;

wire excp_flush;
wire ertn_flush;
wire [31:0] era;
wire [31:0] eentry;

// wire [ 7:0] hard_int_in = 8'b0;
wire        ipi_int_in  = 1'b0;
wire has_int;


if_stage if_stage(
    .clk               (clk              ),
    .reset             (reset            ),

    // allowin
    .ds_allowin        (ds_allowin       ),

    // to ds
    .fs_to_ds_valid    (fs_to_ds_valid   ),
    .fs_to_ds_bus      (fs_to_ds_bus     ),

    .br_bus            (br_bus           ),

    .excp_taken        (excp_flush       ),
    .ertn_taken        (ertn_flush       ),
    .era               (era              ),
    .eentry            (eentry           ),

    // inst sram interface
    .inst_sram_req     (inst_sram_req    ),
    .inst_sram_wstrb   (inst_sram_wstrb  ),
    .inst_sram_addr    (inst_sram_addr   ),
    .inst_sram_wdata   (inst_sram_wdata  ),
    .inst_sram_rdata   (inst_sram_rdata  ),
    .inst_sram_size    (inst_sram_size   ),
    .inst_sram_addr_ok (inst_sram_addr_ok),
    .inst_sram_data_ok (inst_sram_data_ok),
    .inst_sram_wr      (inst_sram_wr     )
);

id_stage id_stage(
    .clk            (clk           ),
    .reset          (reset         ),

    // allowin
    .es_allowin     (es_allowin    ),
    .ds_allowin     (ds_allowin    ),
    
    // from fs
    .fs_to_ds_valid (fs_to_ds_valid),
    .fs_to_ds_bus   (fs_to_ds_bus  ),

    // to es
    .ds_to_es_valid (ds_to_es_valid),
    .ds_to_es_bus   (ds_to_es_bus  ),
    
    .br_bus         (br_bus        ),
    .wb_bus         (wb_bus        ),
    .es_forward     (es_forward    ),
    .ms_forward     (ms_forward    ),
    .ws_forward     (ws_forward    ),

    .excp_flush     (excp_flush    ),
    .ertn_flush     (ertn_flush    ),
    .has_int        (has_int       )
);

exe_stage exe_stage(
    .clk               (clk            ),
    .reset             (reset          ),

    // allowin
    .ms_allowin        (ms_allowin     ),
    .es_allowin        (es_allowin     ),

    // from ds
    .ds_to_es_valid    (ds_to_es_valid ),
    .ds_to_es_bus      (ds_to_es_bus   ),

    // to ms
    .es_to_ms_valid    (es_to_ms_valid ),
    .es_to_ms_bus      (es_to_ms_bus   ),

    // forward
    .es_forward        (es_forward     ),

    // mul div
    .es_div_enable     (es_div_enable  ),
    .es_mul_div_sign   (es_mul_div_sign),
    .es_rj_value       (es_rj_value    ),
    .es_rkd_value      (es_rkd_value   ),
    .div_complete      (div_complete   ),
    
    // data sram interface
    .data_sram_req     (data_sram_req    ),
    .data_sram_wstrb   (data_sram_wstrb  ),
    .data_sram_addr    (data_sram_addr   ),
    .data_sram_wdata   (data_sram_wdata  ),
    .data_sram_size    (data_sram_size   ),
    .data_sram_addr_ok (data_sram_addr_ok),
    .data_sram_wr      (data_sram_wr     ),

    .excp_flush        (excp_flush       ),
    .ertn_flush        (ertn_flush       ),
    .ms_ex             (ms_ex            ),
    .ws_ex             (ws_ex            )
);

mem_stage mem_stage(
    .clk               (clk              ),
    .reset             (reset            ),

    // allowin
    .ws_allowin        (ws_allowin       ),
    .ms_allowin        (ms_allowin       ),

    // from es
    .es_to_ms_valid    (es_to_ms_valid   ),
    .es_to_ms_bus      (es_to_ms_bus     ),

    // to ws
    .ms_to_ws_valid    (ms_to_ws_valid   ),
    .ms_to_ws_bus      (ms_to_ws_bus     ),

    // forward
    .ms_forward        (ms_forward       ),

    // div_mul
    .div_result        (div_result       ),
    .mod_result        (mod_result       ),
    .mul_result        (mul_result       ),

    // from data-sram
    .data_sram_rdata   (data_sram_rdata  ),
    .data_sram_data_ok (data_sram_data_ok),

    .excp_flush        (excp_flush       ),
    .ertn_flush        (ertn_flush       ),
    .ms_ex             (ms_ex            )
);

wb_stage wb_stage(
    .clk               (clk              ),
    .reset             (reset            ),

    // allowin
    .ws_allowin        (ws_allowin       ),

    // from ms
    .ms_to_ws_valid    (ms_to_ws_valid   ),
    .ms_to_ws_bus      (ms_to_ws_bus     ),

    .wb_bus            (wb_bus           ),
    
    // forward
    .ws_forward        (ws_forward       ),

    .excp_flush        (excp_flush       ),
    .ertn_flush        (ertn_flush       ),
    .ws_ex             (ws_ex            ),
    .era               (era              ),
    .eentry            (eentry           ),

    .hard_int_in       (hard_int_in      ),
    .ipi_int_in        (ipi_int_in       ),
    .has_int           (has_int          ),

    // trace debug interface
    .debug_wb_pc       (debug_wb_pc      ),
    .debug_wb_rf_we    (debug_wb_rf_we   ),
    .debug_wb_rf_wnum  (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata (debug_wb_rf_wdata),
    .debug_wb_inst     (debug_wb_inst    )
);

div divider(
    .div_clk    (clk            ),
    .reset      (reset          ),
    .div        (es_div_enable  ),
    .div_signed (es_mul_div_sign),
    .complete   (div_complete   ),
    .x          (es_rj_value    ),
    .y          (es_rkd_value   ),
    .s          (div_result     ),
    .r          (mod_result     )
);

//mul multiplier(
//    .mul_clk    (clk            ),
//    .reset      (reset          ),
//    .mul_signed (es_mul_div_sign),
//    .x          (es_rj_value    ),
//    .y          (es_rkd_value   ),
//    .result     (mul_result     )
//);

mult_gen_0 mul_sign(
    .CLK        (clk            ),
    .A          (es_rj_value    ),
    .B          (es_rkd_value   ),
    .P          (mul_result_sign     ),
    .SCLR       (reset          )
);
mult_gen_1 mul_unsign(
    .CLK        (clk            ),
    .A          (es_rj_value    ),
    .B          (es_rkd_value   ),
    .P          (mul_result_unsign     ),
    .SCLR       (reset          )
);
assign mul_result = es_mul_div_sign ? mul_result_sign : mul_result_unsign;

endmodule
