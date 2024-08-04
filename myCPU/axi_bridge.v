module axi_bridge(  
    input             aclk, 
    input             aresetn,
    // read request channel
    output     [ 3:0] arid,//inst: 0,data: 1
    output     [31:0] araddr,
    output     [ 7:0] arlen,//0
    output     [ 2:0] arsize,
    output     [ 1:0] arburst,//0b01
    output     [ 1:0] arlock,//0
    output     [ 3:0] arcache,//0
    output     [ 2:0] arprot,//0
    output            arvalid,//read address valid
    input             arready,//read address valid
    // read respond channel
    input      [ 3:0] rid,
    input      [31:0] rdata,
    input      [ 1:0] rresp,//ignore
    input             rlast,//ignore
    input             rvalid,//read valid
    output            rready,//read ready
    // write request channel
    output     [ 3:0] awid,//1
    output     [31:0] awaddr,
    output     [ 7:0] awlen,//0
    output     [ 2:0] awsize,
    output     [ 1:0] awburst,//0b01
    output     [ 1:0] awlock,//0
    output     [ 3:0] awcache,//0
    output     [ 2:0] awprot,//0
    output            awvalid,//write address valid
    input             awready,//write address valid
    // write data channel
    output     [ 3:0] wid,//1
    output     [31:0] wdata,
    output     [ 3:0] wstrb,//WSTRB[n] corresponds to WDATA[(8n) + 7: (8n)].
    output            wlast,//1
    output            wvalid,
    input             wready,
    // write respond channel
    input      [ 3:0] bid,//ignore
    input      [ 1:0] bresp,//ignore
    input             bvalid,//write response valid
    output            bready,//write response ready
    // inst sram interface
    input         inst_sram_req,
    input  [ 3:0] inst_sram_wstrb,
    input  [31:0] inst_sram_addr,
    input  [31:0] inst_sram_wdata,
    output [31:0] inst_sram_rdata,
    input  [ 1:0] inst_sram_size,
    output        inst_sram_addr_ok,
    output        inst_sram_data_ok,
    input         inst_sram_wr,//1: write; 0: read
    // data sram interface
    input         data_sram_req,
    input  [ 3:0] data_sram_wstrb,
    input  [31:0] data_sram_addr,
    input  [31:0] data_sram_wdata,
    output [31:0] data_sram_rdata,
    input  [ 1:0] data_sram_size,
    output        data_sram_addr_ok,
    output        data_sram_data_ok,
    input         data_sram_wr
);


/* ------------------- READ FSM ------------------- */

localparam READ_IDLE    = 3'b001; 
localparam READ_RADDR   = 3'b010;      
localparam READ_RDATA   = 3'b100;     

reg [2:0] read_state;
reg [2:0] read_next_state;

// This reg indicate the state of reading from INST RAM
// Because req don't last
// Its opposite -> reading from DATA RAM
reg  reading_inst_ram;
reg  reading_data_ram;

wire read_idle  = read_state[0];
wire read_raddr = read_state[1];
wire read_rdata = read_state[2];

always @ (posedge aclk) begin
    if(~aresetn) begin
        read_state <= READ_IDLE;
    end 
    else begin
        read_state <= read_next_state;
    end
end

always @ (*) begin
    case (read_state)
    READ_IDLE:begin
        if ((data_sram_req && ~data_sram_wr || inst_sram_req && ~inst_sram_wr) && write_idle) begin         
            read_next_state = READ_RADDR;
        end 
        else begin
            read_next_state = READ_IDLE;
        end
    end
    READ_RADDR:begin
        if (arvalid && arready) begin         
            read_next_state = READ_RDATA;
        end 
        else begin
            read_next_state = READ_RADDR;
        end
    end
    READ_RDATA:begin
        if (rready && rvalid) begin
            read_next_state = READ_IDLE;
        end 
        else begin
            read_next_state = READ_RDATA;
        end
    end
    default: 
        read_next_state = READ_IDLE;
    endcase
end

always @(posedge aclk) begin
    if (~aresetn)
        reading_inst_ram <= 1'b0;
    else if (~data_sram_req && read_idle && inst_sram_req && ~inst_sram_wr && write_idle)
        reading_inst_ram <= 1'b1;
    else if (read_rdata && rready && rvalid)
        reading_inst_ram <= 1'b0;
end

always @(posedge aclk) begin
    if (~aresetn)
        reading_data_ram <= 1'b0;
    else if (read_idle && data_sram_req && ~data_sram_wr && write_idle)
        reading_data_ram <= 1'b1;
    else if (read_rdata && rready && rvalid)
        reading_data_ram <= 1'b0;
end

/* -------------------  read request ------------------- */
assign inst_sram_addr_ok = arready && reading_inst_ram;
assign inst_sram_data_ok = rvalid && reading_inst_ram;
assign inst_sram_rdata   = rdata;

wire   finish_two_handshake;

reg [1:0]   two_handshake;
always @(posedge aclk) begin
    if (~aresetn)
        two_handshake <= 2'b00;
    else if (finish_two_handshake)
        two_handshake <= 2'b00;
    else if (awready || wready)
        two_handshake <= two_handshake + 1;
end

assign finish_two_handshake = two_handshake == 2'b10;

// addr_ok: write transactions -> awready and wready (2 handshakes finished)
//          read transactions -> arready
assign data_sram_addr_ok = finish_two_handshake || arready && reading_data_ram;
assign data_sram_data_ok = rvalid && reading_data_ram || bvalid;
assign data_sram_rdata   = rdata;

assign arid    = read_raddr && reading_data_ram;
assign araddr  = {32{read_raddr && reading_inst_ram}} & inst_sram_addr |
                 {32{read_raddr && reading_data_ram}} & data_sram_addr;
assign arsize  = {3{read_raddr && reading_inst_ram}} & {1'b0, inst_sram_size} |
                 {3{read_raddr && reading_data_ram}} & {1'b0, data_sram_size};
assign arvalid = read_raddr;
assign arlen   = 8'b0;
assign arburst = 2'b1;
assign arlock  = 2'b0;
assign arcache = 4'b0;
assign arprot  = 3'b0;

/* -------------------  read respond ------------------- */
assign rready = read_rdata;

/* ------------------- WRITE FSM ------------------- */

localparam WRITE_IDLE    = 4'b0001; 
localparam WRITE_WADDR   = 4'b0010;
localparam WRITE_WDATA   = 4'b0100;      
localparam WRITE_BRESP   = 4'b1000;   
reg [3:0] write_state;
reg [3:0] write_next_state;

wire write_idle = write_state[0];
wire write_waddr = write_state[1];
wire write_wdata = write_state[2];
wire write_bresp = write_state[3];

always @ (posedge aclk) begin
    if(~aresetn) begin
        write_state <= WRITE_IDLE;
    end 
    else begin
        write_state <= write_next_state;
    end
end

always @ (*) begin
    case (write_state)
    WRITE_IDLE:begin
        if (data_sram_req && data_sram_wr) begin         
            write_next_state = WRITE_WADDR;
        end 
        else begin
            write_next_state = WRITE_IDLE;
        end
    end
    WRITE_WADDR:begin
        if (awvalid && awready) begin        
            write_next_state = WRITE_WDATA;
        end 
        else begin
            write_next_state = WRITE_WADDR;
        end
    end
    WRITE_WDATA:begin
        if (wvalid && wready) begin        
            write_next_state = WRITE_BRESP;
        end 
        else begin
            write_next_state = WRITE_WDATA;
        end
    end
    WRITE_BRESP:begin
        if (bvalid && bready) begin
            write_next_state = WRITE_IDLE;
        end 
        else begin
            write_next_state = WRITE_BRESP;
        end
    end
    default: 
        write_next_state = WRITE_IDLE;
    endcase
end

/* -------------------  write request ------------------- */

assign awaddr  = {32{write_waddr}} & data_sram_addr;
assign awsize  = {3{write_waddr}}  & {1'b0, data_sram_size};
assign awvalid = write_waddr;
assign awid    = 4'b1;
assign awlen   = 8'b0;
assign awburst = 2'b1;
assign awlock  = 2'b0;
assign awcache = 4'b0;
assign awprot  = 3'b0;

/* -------------------  write data ------------------- */
assign wdata   = {32{write_wdata}} & data_sram_wdata;
assign wstrb   = {4{write_wdata}}  & data_sram_wstrb;
assign wvalid  = write_wdata;
assign wid     = 4'b1;
assign wlast   = 1'b1;

/* -------------------  write respond ------------------- */
assign bready  = write_bresp; 

endmodule