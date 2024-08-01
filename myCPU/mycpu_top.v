module mycpu_top(// AXI structure and cache according to CALab
    input         aclk,
    input         aresetn,
    
    // AXI bridge interface
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

    // trace debug interface
    output [31:0] debug_wb_pc,
    output [ 3:0] debug_wb_rf_we,
    output [ 4:0] debug_wb_rf_wnum,
    output [31:0] debug_wb_rf_wdata
);

     // inst sram interface
    wire        inst_sram_req;
    wire        inst_sram_wr;
    wire [ 1:0] inst_sram_size;
    wire [ 3:0] inst_sram_wstrb;
    wire [31:0] inst_sram_addr;
    wire [31:0] inst_sram_wdata;
    wire        inst_sram_addr_ok;
    wire        inst_sram_data_ok;
    wire [31:0] inst_sram_rdata;
    // data sram interface
    wire        data_sram_req;
    wire        data_sram_wr;
    wire [ 1:0] data_sram_size;
    wire [ 3:0] data_sram_wstrb;
    wire [31:0] data_sram_addr;
    wire [31:0] data_sram_wdata;
    wire        data_sram_addr_ok;
    wire        data_sram_data_ok;
    wire [31:0] data_sram_rdata;

    //icache read channel , we don't have a write channel for icache
    wire        icache_addr_ok;
    wire        icache_data_ok;
    wire [31:0] icache_rdata;
    wire        icache_rd_req;
    wire [ 2:0] icache_rd_type;
    wire [31:0] icache_rd_addr;
    wire        icache_rd_rdy;
    wire        icache_ret_valid;
    wire        icache_ret_last;
    wire [31:0] icache_ret_data;
     //icache write channel=meaning less ,all is 0        
    wire        icache_wr_req;
    wire [ 2:0] icache_wr_type;
    wire [31:0] icache_wr_addr;
    wire [ 3:0] icache_wr_strb;
    wire [127:0]icache_wr_data;
    wire        icache_wr_rdy=1'b0;     

    //dcache read channel
    wire        dcache_addr_ok;
    wire        dcache_data_ok;
    wire [31:0] dcache_rdata;
    wire        dcache_rd_req;
    wire [ 2:0] dcache_rd_type;
    wire [31:0] dcache_rd_addr;
    wire        dcache_rd_rdy;
    wire        dcache_ret_valid;
    wire        dcache_ret_last;
    wire [31:0] dcache_ret_data;

    //dcache write channel
    wire        dcache_wr_req;
    wire [ 2:0] dcache_wr_type;
    wire [31:0] dcache_wr_addr;
    wire [ 3:0] dcache_wr_strb;
    wire[127:0] dcache_wr_data;
    wire        dcache_wr_rdy;
    //exp21: 继承指令cache

axi_bridge my_bridge_sram_axi(
    .aclk               (aclk               ),
    .aresetn            (aresetn            ),

    .arid               (arid               ),
    .araddr             (araddr             ),
    .arlen              (arlen              ),
    .arsize             (arsize             ),
    .arburst            (arburst            ),
    .arlock             (arlock             ),
    .arcache            (arcache            ),
    .arprot             (arprot             ),
    .arvalid            (arvalid            ),
    .arready            (arready            ),

    .rid                (rid                ),
    .rdata              (rdata              ),
    .rvalid             (rvalid             ),
    .rlast              (rlast              ),
    .rready             (rready             ),

    .awid               (awid               ),
    .awaddr             (awaddr             ),
    .awlen              (awlen              ),
    .awsize             (awsize             ),
    .awburst            (awburst            ),
    .awlock             (awlock             ),
    .awcache            (awcache            ),
    .awprot             (awprot             ),
    .awvalid            (awvalid            ),
    .awready            (awready            ),

    .wid                (wid                ),
    .wdata              (wdata              ),
    .wstrb              (wstrb              ),
    .wlast              (wlast              ),
    .wvalid             (wvalid             ),
    .wready             (wready             ),

    .bid                (bid                ),
    .bvalid             (bvalid             ),
    .bready             (bready             ),

    .icache_rd_req      (icache_rd_req      ),
    .icache_rd_type     (icache_rd_type     ),
    .icache_rd_addr     (icache_rd_addr     ),
    .icache_rd_rdy      (icache_rd_rdy      ),
    .icache_ret_valid   (icache_ret_valid   ),
    .icache_ret_last    (icache_ret_last    ),
    .icache_ret_data    (icache_ret_data    ),

    .dcache_rd_req      (dcache_rd_req      ),
    .dcache_rd_type     (dcache_rd_type     ),
    .dcache_rd_addr     (dcache_rd_addr     ),
    .dcache_rd_rdy      (dcache_rd_rdy      ),
    .dcache_ret_valid   (dcache_ret_valid   ),
    .dcache_ret_last    (dcache_ret_last    ),
    .dcache_ret_data    (dcache_ret_data    ),

    .dcache_wr_req      (dcache_wr_req      ),
    .dcache_wr_type     (dcache_wr_type     ),
    .dcache_wr_addr     (dcache_wr_addr     ),
    .dcache_wr_wstrb    (dcache_wr_wstrb    ),
    .dcache_wr_data     (dcache_wr_data     ),
    .dcache_wr_rdy      (dcache_wr_rdy      )
    );





cpu_core cpu_core(
    .clk                (aclk               ),
    .resetn             (aresetn            ),
    // inst sram interface
    .inst_sram_req      (inst_sram_req     ),
    .inst_sram_wr       (inst_sram_wr      ),
    .inst_sram_size     (inst_sram_size    ),
    .inst_sram_wstrb    (inst_sram_wstrb   ),
    .inst_sram_addr     (inst_sram_addr    ),
    .inst_sram_wdata    (inst_sram_wdata   ),
    .inst_sram_addr_ok  (icache_addr_ok    ),//icache add
    .inst_sram_data_ok  (icache_data_ok    ),
    .inst_sram_rdata    (icache_rdata      ),
    // data sram interface
    .data_sram_req      (data_sram_req     ),
    .data_sram_wstrb    (data_sram_wstrb   ),
    .data_sram_addr     (data_sram_addr    ),
    .data_sram_wdata    (data_sram_wdata   ),
    .data_sram_rdata    (dcache_rdata   ),
    .data_sram_size     (data_sram_size    ),
    .data_sram_addr_ok  (dcache_addr_ok ),
    .data_sram_data_ok  (dcache_data_ok ),
    .data_sram_wr       (data_sram_wr      ),
    // trace debug interface
    .debug_wb_pc        (debug_wb_pc      ),
    .debug_wb_rf_we     (debug_wb_rf_we   ),
    .debug_wb_rf_wnum   (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata  (debug_wb_rf_wdata)
);

    cache Icache(
        //----------cpu interface------
        .clk    (aclk                       ),
        .resetn (aresetn                    ),
        .valid  (inst_sram_req              ),//pre-if request valid
        .op     (inst_sram_wr               ),//always 0==read
        .index  (inst_sram_addr[11:4]       ),
        .tag    (inst_sram_addr[31:12]      ),//from tlb:inst_sram_addr[31:12]=实地�?
        .offset (inst_sram_addr[3:0]        ),
        .wstrb  (inst_sram_wstrb            ),
        .wdata  (inst_sram_wdata            ),
        .addr_ok(icache_addr_ok             ),//output 流水线方�? 阻塞流水线的指令
        .data_ok(icache_data_ok             ),
        .rdata  (icache_rdata               ),//output
        //--------AXI read interface-------
        .rd_req (icache_rd_req              ),//output
        .rd_type(icache_rd_type             ),
        .rd_addr(icache_rd_addr             ),

        .rd_rdy   (icache_rd_rdy            ),//input 总线发来�?
        .ret_valid(icache_ret_valid         ),
        .ret_last (icache_ret_last          ),
        .ret_data (icache_ret_data          ),

        //--------AXI write interface------
        .wr_req (icache_wr_req              ),//output,对于icache永远�?0
        .wr_type(icache_wr_type             ),
        .wr_addr(icache_wr_addr             ),
        .wr_wstrb(icache_wr_strb             ),
        .wr_data(icache_wr_data             ),
        .wr_rdy (icache_wr_rdy              )//icache不会真正要写sram，置1没有关系
    );
    
 cache Dcache(
        //----------cpu interface------
        .clk    (aclk                       ),
        .resetn (aresetn                    ),
        .valid  (data_sram_req              ),//pre-if request valid
        .op     (data_sram_wr               ),//always 0==read
        .index  (data_sram_addr[11:4]       ),
        .tag    (data_sram_addr[31:12]      ),//from tlb:inst_sram_addr[31:12]=实地�?
        .offset (data_sram_addr[3:0]        ),
        .wstrb  (data_sram_wstrb            ),
        .wdata  (data_sram_wdata            ),
        .addr_ok(dcache_addr_ok             ),//output 流水线方�? 阻塞流水线的指令
        .data_ok(dcache_data_ok             ),
        .rdata  (dcache_rdata               ),//output
        //--------AXI read interface-------
        .rd_req (dcache_rd_req              ),//output
        .rd_type(dcache_rd_type             ),
        .rd_addr(dcache_rd_addr             ),

        .rd_rdy   (dcache_rd_rdy            ),//input 总线发来�?
        .ret_valid(dcache_ret_valid         ),
        .ret_last (dcache_ret_last          ),
        .ret_data (dcache_ret_data          ),

        //--------AXI write interface------
        .wr_req (dcache_wr_req              ),//output,
        .wr_type(dcache_wr_type             ),
        .wr_addr(dcache_wr_addr             ),
        .wr_wstrb(dcache_wr_strb            ),
        .wr_data(dcache_wr_data             ),
        .wr_rdy (dcache_wr_rdy              )
    );


endmodule
