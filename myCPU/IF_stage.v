`include "myCPU.h"

module if_stage(
    input  clk,
    input  reset,

    // allowin
    input  ds_allowin,

    // to ds
    output fs_to_ds_valid,
    output [`FS_TO_DS_BUS_WD-1:0] fs_to_ds_bus,

    input  [`BR_BUS_WD-1:0] br_bus,

    input  excp_taken,
    input  ertn_taken,
    input  [31:0] era,
    input  [31:0] eentry,

    // to btb
    output [31:0]                  fetch_pc          ,
    output                         fetch_en          ,
    input  [31:0]                  btb_ret_pc        ,
    input                          btb_taken         ,
    input                          btb_en            ,
    input  [ 4:0]                  btb_index         ,

    // inst sram interface
    output inst_sram_req,
    output [ 3:0] inst_sram_wstrb,
    output [31:0] inst_sram_addr,
    output [31:0] inst_sram_wdata,
    input  [31:0] inst_sram_rdata,
    output [ 1:0] inst_sram_size,
    input  inst_sram_addr_ok,
    input  inst_sram_data_ok,
    output inst_sram_wr
);

wire br_taken_r;
wire br_taken;
wire [31:0] br_target;

wire ps_stall;
wire ps_ready_go;
wire ps_to_fs_valid;

wire fs_flush;
wire fs_stall;
wire fs_ready_go;
wire fs_allowin;
reg  fs_valid;

reg  excp_taken_buf;
reg  ertn_taken_buf;
reg  br_taken_buf;
reg  [31:0] eentry_buf;
reg  [31:0] era_buf;
reg  [31:0] br_target_buf;

wire [31:0] seq_pc;
reg [31:0] nextpc;
//wire [31:0] nextpc_buf;

wire excp_flush;
wire ertn_flush;
wire br_flush;

reg  [31:0] fs_pc;
reg  [31:0] fs_inst_buf;
reg  fs_inst_buf_valid;
wire [31:0] fs_inst;

wire         btb_pre_error_flush;
wire [31:0]  btb_pre_error_flush_target;

wire excp_adef;
wire ps_excp;
reg  fs_excp_r;
wire fs_excp;
wire [15:0] ps_excp_num;
reg  [15:0] fs_excp_num;

reg  data_arrived;
wire excp_noin;
wire excp_noin_r;

assign excp_noin_r = !(!ps_ready_go && (!ds_allowin || !fs_ready_go)) ? 1'b1 : 1'b0;
assign excp_noin = excp_noin_r ===1'bx ? 1'b0 : excp_noin_r;

wire  [31:0] btb_ret_pc_t;
wire  [ 4:0] btb_index_t;
wire         btb_taken_t;
wire         btb_en_t;

/*
* br state machine
* when btb pre error, id stage will cancel one inst. so need confirm useless
* inst (will be canceled) is generated. 
*/ 
reg [31:0] br_target_inst_req_buffer;
reg [ 2:0] br_target_inst_req_state;
localparam br_target_inst_req_empty = 3'b001;
localparam br_target_inst_req_wait_slot = 3'b010;
localparam br_target_inst_req_wait_br_target = 3'b100;

always @(posedge clk) begin
    if (reset) begin
        br_target_inst_req_state <= br_target_inst_req_empty;
    end
    else case (br_target_inst_req_state) 
        br_target_inst_req_empty: begin
            if (fs_flush) begin
                br_target_inst_req_state <= br_target_inst_req_empty; 
            end
            else if(btb_pre_error_flush && !fs_valid) begin
                br_target_inst_req_state  <= br_target_inst_req_wait_slot;
                br_target_inst_req_buffer <= btb_pre_error_flush_target;
            end
            else if(btb_pre_error_flush && fs_valid || btb_pre_error_flush && !fs_valid) begin
                br_target_inst_req_state  <= br_target_inst_req_wait_br_target;
                br_target_inst_req_buffer <= btb_pre_error_flush_target;
            end
        end
        br_target_inst_req_wait_slot: begin
            if(fs_flush) begin
                br_target_inst_req_state <= br_target_inst_req_empty;
            end
            else if(ps_ready_go) begin
                br_target_inst_req_state <= br_target_inst_req_wait_br_target;
            end
        end
        br_target_inst_req_wait_br_target: begin
            if(ps_ready_go || fs_flush) begin
                br_target_inst_req_state <= br_target_inst_req_empty;
            end
        end
        default: begin
            br_target_inst_req_state <= br_target_inst_req_empty;
        end
    endcase
end

/*
* btb lock
* btb ret only maintain one clock
* when pfs not ready go, should buffer btb ret
*/
reg [37:0] btb_lock_buffer;
reg        btb_lock_en;
always @(posedge clk) begin
	if (reset || fs_flush || fetch_en)
		btb_lock_en <= 1'b0;
	else if (btb_en && !ps_ready_go) begin
		btb_lock_en     <= 1'b1;
		btb_lock_buffer <= {btb_taken, btb_index, btb_ret_pc};
	end
end

assign btb_ret_pc_t = {32{btb_lock_en}} & btb_lock_buffer[31:0] | btb_ret_pc;
assign btb_index_t  = {5{btb_lock_en}} & btb_lock_buffer[36:32] | btb_index;
assign btb_taken_t  = btb_lock_en && btb_lock_buffer[37] || btb_taken;
assign btb_en_t     = btb_lock_en || btb_en;

// pre-IF stage
// assign {
//     br_taken_r,
//     br_target
// } = br_bus;

assign {btb_pre_error_flush, btb_pre_error_flush_target} = br_bus;

// assign br_taken = br_taken_r === 1'bx ? 1'b0 :  br_taken_r;
assign fetch_btb_target = (btb_taken && btb_en) || (btb_lock_en && btb_lock_buffer[37]);
assign ps_ready_go    = inst_sram_req && inst_sram_addr_ok;
assign ps_to_fs_valid = ps_ready_go;

always @(posedge clk) begin
    if (reset) begin
        excp_taken_buf <= 1'b0;
        ertn_taken_buf <= 1'b0;
        //excp_noin      <= 1'b0;
    end
    //else if (!ps_ready_go && (!ds_allowin || !fs_ready_go)) begin
    //else if (ps_ready_go) begin
        else if (excp_taken) begin
            excp_taken_buf <= 1'b1;
            eentry_buf     <= eentry;
            //excp_noin      <= 1'b1;
        end
        else if (ertn_taken) begin
            ertn_taken_buf <= 1'b1;
            era_buf        <= era;
        end
    //end
    else if (ps_ready_go) begin
        excp_taken_buf <= 1'b0;
        ertn_taken_buf <= 1'b0;
    end
end

// always @(posedge clk) begin
// if (reset) begin
//         br_taken_buf   <= 1'b0;
//     end
//     else if (/*!ps_ready_go &&*/br_taken && (!ds_allowin || !fs_ready_go)) begin
//             br_taken_buf   <= 1'b1;
//             br_target_buf  <= br_target;
//     end
//     else if (ps_ready_go) begin
//         br_taken_buf   <= 1'b0;
//     end
// end
assign seq_pc = fs_pc + 32'h4;
//assign nextpc = fs_excp        ? 32'h1c000000 :
//                excp_noin      ? nextpc :
//                excp_taken_buf ? eentry_buf :
//                excp_taken     ? eentry :
//                ertn_taken_buf ? era_buf :
//                ertn_taken     ? era :
//                br_taken_buf   ? br_target_buf :
//                br_taken       ? br_target :
//                seq_pc;
always @(*) begin
    if(reset) begin
        nextpc = seq_pc;
    end  
    else if(fs_excp) begin
        nextpc = 32'h1c000000;
    end  
    else if(excp_noin) begin
        nextpc = nextpc;
    end
    else if(excp_taken_buf) begin
        nextpc = eentry_buf;
    end     
    else if(excp_taken) begin
        nextpc = eentry;
    end  
    else if(ertn_taken_buf) begin
        nextpc = era_buf;
    end
    else if(ertn_taken) begin
        nextpc = era;
    end   
    else if(btb_pre_error_flush && fs_valid) begin
        nextpc = btb_pre_error_flush_target;
    end
    else begin
           nextpc = seq_pc;
    end
end
//assign nextpc = ps_ready_go ? nextpc : nextpc_buf;

assign inst_sram_req   = fs_allowin && !ps_excp;
assign inst_sram_wstrb = 4'b0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0;
assign inst_sram_size  = 2'b10;
assign inst_sram_wr    = 1'b0;

assign excp_adef = nextpc[1:0] != 2'b00;
assign ps_excp     = excp_adef;
assign ps_excp_num = {1'b0, excp_adef, 14'b0};

assign excp_flush = (excp_taken_buf || excp_taken) && ds_allowin && data_arrived;
assign ertn_flush = (ertn_taken_buf || ertn_taken) && ds_allowin && data_arrived;
assign br_flush   = (btb_pre_error_flush) && ds_allowin && data_arrived;

// IF stage
assign fs_flush       = excp_flush || ertn_flush || br_flush;
assign fs_stall       = !data_arrived && !fs_excp;
assign fs_ready_go    = !fs_flush && !fs_stall ;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin || fs_flush;
assign fs_to_ds_valid = fs_valid && fs_ready_go ||  fs_excp;
assign fs_excp = fs_excp_r === 1'bx ? 1'b0 : fs_excp_r;
always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= ps_to_fs_valid;
    end

    if (reset) begin
        fs_pc <= 32'h1bfffffc;  //trick: to make nextpc be 0xbfc00000 during reset
    end
    else if ((ps_to_fs_valid || ps_excp) && fs_allowin) begin
        fs_pc <= nextpc;
        fs_excp_r <= ps_excp;
        fs_excp_num <= ps_excp_num;
    end
end

always @(posedge clk) begin
    if (reset) begin
        data_arrived <= 1'b0;
    end
    else if (inst_sram_data_ok) begin
        data_arrived <= 1'b1;
    end
    else if (data_arrived && fs_ready_go && ds_allowin || fs_flush) begin
        data_arrived <= 1'b0;
    end
end

always @(posedge clk) begin
    if (reset) begin
        fs_inst_buf <= 32'b0;
        fs_inst_buf_valid <= 1'b0;
    end
    else if(fs_ready_go && ds_allowin) begin
        fs_inst_buf_valid <= 1'b0;
    end
    else if (inst_sram_data_ok && (!ds_allowin || !fs_ready_go)) begin
        fs_inst_buf <= inst_sram_rdata;
        fs_inst_buf_valid <= 1'b1;
    end
end

assign fs_inst = fs_inst_buf_valid ? fs_inst_buf : inst_sram_rdata;

assign fs_to_ds_bus = {
    fs_pc,
    fs_inst,
    fs_excp,
    fs_excp_num,
    btb_ret_pc_t,
    btb_index_t,
    btb_taken_t,
    btb_en_t
};

endmodule
