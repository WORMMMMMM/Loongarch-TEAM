`include "myCPU.h"

module if_stage(
    input                          clk            ,
    input                          reset          ,
    //allwoin
    input                          ds_allowin     ,
    //brbus
    input  [`BR_BUS_WD       -1:0] br_bus         ,
    //to ds
    output                         fs_to_ds_valid ,
    output [`FS_TO_DS_BUS_WD -1:0] fs_to_ds_bus   ,
    // inst sram interface
    output        inst_sram_req  ,
    output [ 3:0] inst_sram_wstrb,
    output [ 1:0] inst_sram_size,
    output        inst_sram_en   ,
    output [ 3:0] inst_sram_we  ,
    output [31:0] inst_sram_addr ,
    output [31:0] inst_sram_wdata,
    input  [31:0] inst_sram_rdata                             //输入和读取是否有问题�???
);

reg         fs_valid;
wire        fs_ready_go;
wire        fs_allowin;
wire        to_fs_valid;

wire [31:0] seq_pc;
wire [31:0] nextpc;

wire         br_taken;
wire         br_taken_r;
wire [ 31:0] br_target;
assign {br_taken_r,br_target} = br_bus;
assign br_taken = (br_taken_r !==1'bx) ? br_taken_r : 1'b0;

wire [31:0] fs_inst;
reg  [31:0] fs_pc;
assign fs_to_ds_bus = {fs_inst ,
                       fs_pc   };
                       
assign inst_sram_req   = fs_allowin; //req

// pre-IF stage
assign to_fs_valid  = ~reset;
assign seq_pc       = fs_pc + 3'h4;
assign nextpc       = br_taken ? br_target : seq_pc; 

// IF stage
assign fs_ready_go    = 1'b1;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;
assign fs_to_ds_valid =  fs_valid && fs_ready_go; 
always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= to_fs_valid;
    end

    if (reset) begin
        fs_pc <= 32'h1bfffffc;  //trick: to make nextpc be 0xbfc00000 during reset +
    end
    else if (to_fs_valid && fs_allowin) begin
        fs_pc <= nextpc;
    end
end

assign inst_sram_size  = 2'b10;
assign inst_sram_en    = to_fs_valid && fs_allowin;
assign inst_sram_we   = 4'h0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0;

assign inst_sram_wstrb = 4'b0;

assign fs_inst         = br_taken ? 32'h02800000 : inst_sram_rdata;

endmodule
