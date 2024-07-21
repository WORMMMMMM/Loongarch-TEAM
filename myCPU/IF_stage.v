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
    output                         inst_sram_req  ,
    output [ 3:0]                  inst_sram_wstrb,
    output [ 1:0]                  inst_sram_size,
    input                          inst_sram_addr_ok,
    input                          inst_sram_data_ok,
    output                         inst_sram_wr,
    output [31:0]                  inst_sram_addr ,
    output [31:0]                  inst_sram_wdata,
    input  [31:0]                  inst_sram_rdata                             //输入和读取是否有问题�???
);

reg         fs_valid;
wire        fs_ready_go;
wire        fs_allowin;
wire        to_fs_valid;
wire        ps_ready_go;

wire [31:0] seq_pc;
wire [31:0] nextpc;
wire [31:0] final_nextpc;
wire        br_taken;
wire        br_taken_r;
reg         br_taken_buf;
wire        br_stall;
reg  [31:0] nextpc_buf;
wire [ 31:0] br_target;
reg [31:0] fs_inst_buf;
reg        fs_inst_buf_valid;
reg        mid_handshake;

wire [31:0] fs_inst;
reg  [31:0] fs_pc;

// pre-IF stage
assign to_fs_valid  = ~reset&&ps_ready_go;
assign seq_pc       = fs_pc + 3'h4;
assign nextpc       = br_taken ? br_target : seq_pc;
assign final_nextpc = br_taken_buf ? nextpc_buf : nextpc; 
assign ps_ready_go  = inst_sram_req&&inst_sram_addr_ok;
/*---------------------Signal interface---------------------*/
assign {br_stall,br_taken_r,br_target} = br_bus;
assign fs_to_ds_bus = {fs_inst ,
                       fs_pc   };
assign br_taken = (br_taken_r !==1'bx) ? br_taken_r : 1'b0;

/*---------------------Handshaking---------------------*/
assign fs_ready_go    = inst_sram_data_ok || fs_inst_buf_valid;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;
assign fs_to_ds_valid =  fs_valid && fs_ready_go; 
always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= to_fs_valid;
    end
end
always @(posedge clk) begin
    if (reset) begin
        fs_pc <= 32'h1bfffffc;  //trick: to make nextpc be 0xbfc00000 during reset +
    end
    else if (to_fs_valid && fs_allowin) begin
        fs_pc <= final_nextpc;
    end
end
//set mid-handshake signal to debug ; it 's set for 1 when addr is ok and req is high but data is not ok
always @(posedge clk) begin
    if (reset)
        mid_handshake <= 1'b0;
    else if (inst_sram_data_ok)
        mid_handshake <= 1'b0;
    else if (inst_sram_req && inst_sram_addr_ok)
        mid_handshake <= 1'b1;
end
/*-----------------Buffer for inst_sram_rdata-----------------*/
always @(posedge clk) begin
    if(reset) begin
        fs_inst_buf <= 32'b0; 
    end
    else if(inst_sram_data_ok && ~ds_allowin) begin
        fs_inst_buf <= inst_sram_rdata;
    end
end

always @(posedge clk) begin
    if(reset) begin
        fs_inst_buf_valid <= 1'b0;
    end
    else if(fs_ready_go && ds_allowin) begin
        fs_inst_buf_valid <= 1'b0;
    end
    else if(inst_sram_data_ok && ~ds_allowin) begin
        fs_inst_buf_valid <= 1'b1;
    end
end
always @(posedge clk) begin
    if(reset) begin
        nextpc_buf <= 32'b0;
    end
    else if(br_taken && ~br_stall) begin
        nextpc_buf <= nextpc;
    end
end
always @(posedge clk)begin
    if(reset) begin
        br_taken_buf <= 1'b0;
    end
    else if(br_taken_buf && inst_sram_req && inst_sram_addr_ok && fs_allowin) begin
        br_taken_buf <= 1'b0;
    end 
    else if(br_taken && ~br_stall && ~(inst_sram_req && inst_sram_addr_ok)) begin
        br_taken_buf <= br_taken;
    end
end


assign inst_sram_req   = fs_allowin && ~br_stall; //req
assign inst_sram_size  = 2'b10;
assign inst_sram_addr  = final_nextpc;
assign inst_sram_wdata = 32'b0;
assign inst_sram_wr    = 1'b0;
assign inst_sram_wstrb = 4'b0;

assign fs_inst         = 
                         fs_inst_buf_valid ? fs_inst_buf:
                         inst_sram_rdata;

endmodule
