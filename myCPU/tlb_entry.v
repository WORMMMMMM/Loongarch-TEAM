module tlb_entry
#(
    parameter TLBNUM = 32
)
(
    input  wire      clk,
    // search port 0
    input  wire                      s0_fetch    ,
    input  wire [18:0]               s0_vppn     ,
    input  wire                      s0_odd_page ,
    input  wire [ 9:0]               s0_asid     ,
    output reg                       s0_found    ,
    output reg  [ 4:0]               s0_index    ,
    output reg  [ 5:0]               s0_ps       ,
    output reg  [19:0]               s0_ppn      ,
    output reg                       s0_v        ,
    output reg                       s0_d        ,
    output reg  [ 1:0]               s0_mat      ,
    output reg  [ 1:0]               s0_plv      ,
    //search port 1
    input  wire                      s1_fetch    ,
    input  wire [18:0]               s1_vppn     ,
    input  wire                      s1_odd_page ,
    input  wire [ 9:0]               s1_asid     ,
    output reg                       s1_found    ,
    output reg  [ 4:0]               s1_index    ,
    output reg  [ 5:0]               s1_ps       ,
    output reg  [19:0]               s1_ppn      ,
    output reg                       s1_v        ,
    output reg                       s1_d        ,
    output reg  [ 1:0]               s1_mat      ,
    output reg  [ 1:0]               s1_plv      ,
    // write port 
    input  wire                      we          ,
    input  wire [$clog2(TLBNUM)-1:0] w_index     ,
    input  wire [18:0]               w_vppn      ,
    input  wire [ 9:0]               w_asid      ,
    input  wire                      w_g         ,
    input  wire [ 5:0]               w_ps        ,
    input  wire                      w_e         ,
    input  wire                      w_v0        ,
    input  wire                      w_d0        ,
    input  wire [ 1:0]               w_mat0      ,
    input  wire [ 1:0]               w_plv0      ,
    input  wire [19:0]               w_ppn0      ,
    input  wire                      w_v1        ,
    input  wire                      w_d1        ,
    input  wire [ 1:0]               w_mat1      ,
    input  wire [ 1:0]               w_plv1      ,
    input  wire [19:0]               w_ppn1      ,
    // read port
    input  wire [$clog2(TLBNUM)-1:0] r_index     ,
    output wire [18:0]               r_vppn      ,
    output wire [ 9:0]               r_asid      ,
    output wire                      r_g         ,
    output wire [ 5:0]               r_ps        ,
    output wire                      r_e         ,
    output wire                      r_v0        ,
    output wire                      r_d0        ,
    output wire [ 1:0]               r_mat0      ,
    output wire [ 1:0]               r_plv0      ,
    output wire [19:0]               r_ppn0      ,
    output wire                      r_v1        ,
    output wire                      r_d1        ,
    output wire [ 1:0]               r_mat1      ,
    output wire [ 1:0]               r_plv1      ,
    output wire [19:0]               r_ppn1      ,
    // invalid port 
    input  wire                      inv_en      ,
    input  wire [ 4:0]               inv_op      ,
    input  wire [ 9:0]               inv_asid    ,
    input  wire [18:0]               inv_vpn
);

reg [18:0] tlb_vppn     [TLBNUM-1:0];
reg        tlb_e        [TLBNUM-1:0];
reg [ 9:0] tlb_asid     [TLBNUM-1:0];
reg        tlb_g        [TLBNUM-1:0];
reg [ 5:0] tlb_ps       [TLBNUM-1:0];
reg [19:0] tlb_ppn0     [TLBNUM-1:0];
reg [ 1:0] tlb_plv0     [TLBNUM-1:0];
reg [ 1:0] tlb_mat0     [TLBNUM-1:0];
reg        tlb_d0       [TLBNUM-1:0];
reg        tlb_v0       [TLBNUM-1:0];
reg [19:0] tlb_ppn1     [TLBNUM-1:0];
reg [ 1:0] tlb_plv1     [TLBNUM-1:0];
reg [ 1:0] tlb_mat1     [TLBNUM-1:0];
reg        tlb_d1       [TLBNUM-1:0];
reg        tlb_v1       [TLBNUM-1:0];

wire [TLBNUM-1:0] match0;
wire [TLBNUM-1:0] match1;

wire [$clog2(TLBNUM)-1:0] match0_en;
wire [$clog2(TLBNUM)-1:0] match1_en;

wire [TLBNUM-1:0] s0_odd_page_buffer;
wire [TLBNUM-1:0] s1_odd_page_buffer;

wire s0_found_t, s1_found_t;

genvar i;
generate
    for (i = 0; i < TLBNUM; i = i + 1)
        begin: match
            assign s0_odd_page_buffer[i] = (tlb_ps[i] == 6'd12) ? s0_odd_page : s0_vppn[8];
            assign match0[i] = (tlb_e[i] == 1'b1) && ((tlb_ps[i] == 6'd12) ? s0_vppn == tlb_vppn[i] : s0_vppn[18: 9] == tlb_vppn[i][18: 9]) && ((s0_asid == tlb_asid[i]) || tlb_g[i]);
            assign s1_odd_page_buffer[i] = (tlb_ps[i] == 6'd12) ? s1_odd_page : s1_vppn[8];
            assign match1[i] = (tlb_e[i] == 1'b1) && ((tlb_ps[i] == 6'd12) ? s1_vppn == tlb_vppn[i] : s1_vppn[18: 9] == tlb_vppn[i][18: 9]) && ((s1_asid == tlb_asid[i]) || tlb_g[i]);
        end
endgenerate

encoder_32_5 en_match0 (.in({{(32-TLBNUM){1'b0}},match0}), .out(match0_en));
encoder_32_5 em_match1 (.in({{(32-TLBNUM){1'b0}},match1}), .out(match1_en));

assign s0_found_t = |match0;
assign s1_found_t = |match1;

always @(posedge clk) begin
    if (s0_fetch)
	s0_found <= s0_found_t;
	
	if (s0_found_t && s0_fetch) begin
		s0_index <= {{(5-$clog2(TLBNUM)){1'b0}},match0_en};
		s0_ps    <= tlb_ps[match0_en];
		s0_ppn   <= s0_odd_page_buffer[match0_en] ? tlb_ppn1[match0_en] : tlb_ppn0[match0_en];
		s0_v     <= s0_odd_page_buffer[match0_en] ? tlb_v1[match0_en]   : tlb_v0[match0_en]  ;
		s0_d     <= s0_odd_page_buffer[match0_en] ? tlb_d1[match0_en]   : tlb_d0[match0_en]  ;
		s0_mat   <= s0_odd_page_buffer[match0_en] ? tlb_mat1[match0_en] : tlb_mat0[match0_en];
		s0_plv   <= s0_odd_page_buffer[match0_en] ? tlb_plv1[match0_en] : tlb_plv0[match0_en];
	end
    
    if (s1_fetch)
	s1_found <= s1_found_t;
	
	if (s1_found_t && s1_fetch) begin
		s1_index <= {{(5-$clog2(TLBNUM)){1'b0}},match1_en};
		s1_ps    <= tlb_ps[match1_en];
		s1_ppn   <= s1_odd_page_buffer[match1_en] ? tlb_ppn1[match1_en] : tlb_ppn0[match1_en];
		s1_v     <= s1_odd_page_buffer[match1_en] ? tlb_v1[match1_en]   : tlb_v0[match1_en]  ;
		s1_d     <= s1_odd_page_buffer[match1_en] ? tlb_d1[match1_en]   : tlb_d0[match1_en]  ;
		s1_mat   <= s1_odd_page_buffer[match1_en] ? tlb_mat1[match1_en] : tlb_mat0[match1_en];
		s1_plv   <= s1_odd_page_buffer[match1_en] ? tlb_plv1[match1_en] : tlb_plv0[match1_en];
	end
end

always @(posedge clk) begin
    if (we) begin
        tlb_vppn [w_index] <= w_vppn;
        tlb_asid [w_index] <= w_asid;
        tlb_g    [w_index] <= w_g; 
        tlb_ps   [w_index] <= w_ps;  
        tlb_ppn0 [w_index] <= w_ppn0;
        tlb_plv0 [w_index] <= w_plv0;
        tlb_mat0 [w_index] <= w_mat0;
        tlb_d0   [w_index] <= w_d0;
        tlb_v0   [w_index] <= w_v0; 
        tlb_ppn1 [w_index] <= w_ppn1;
        tlb_plv1 [w_index] <= w_plv1;
        tlb_mat1 [w_index] <= w_mat1;
        tlb_d1   [w_index] <= w_d1;
        tlb_v1   [w_index] <= w_v1; 
    end
end

assign r_vppn  =  tlb_vppn [r_index]; 
assign r_asid  =  tlb_asid [r_index]; 
assign r_g     =  tlb_g    [r_index]; 
assign r_ps    =  tlb_ps   [r_index]; 
assign r_e     =  tlb_e    [r_index]; 
assign r_v0    =  tlb_v0   [r_index]; 
assign r_d0    =  tlb_d0   [r_index]; 
assign r_mat0  =  tlb_mat0 [r_index]; 
assign r_plv0  =  tlb_plv0 [r_index]; 
assign r_ppn0  =  tlb_ppn0 [r_index]; 
assign r_v1    =  tlb_v1   [r_index]; 
assign r_d1    =  tlb_d1   [r_index]; 
assign r_mat1  =  tlb_mat1 [r_index]; 
assign r_plv1  =  tlb_plv1 [r_index]; 
assign r_ppn1  =  tlb_ppn1 [r_index]; 

//tlb entry invalid 
generate 
    for (i = 0; i < TLBNUM; i = i + 1) 
        begin: invalid_tlb_entry 
            always @(posedge clk) begin
                if (we && (w_index == i)) begin
                    tlb_e[i] <= w_e;
                end
                else if (inv_en) begin
                    if (inv_op == 5'd0 || inv_op == 5'd1) begin
                        tlb_e[i] <= 1'b0;
                    end
                    else if (inv_op == 5'd2) begin
                        if (tlb_g[i]) begin
                            tlb_e[i] <= 1'b0;
                        end
                    end
                    else if (inv_op == 5'd3) begin
                        if (!tlb_g[i]) begin
                            tlb_e[i] <= 1'b0;
                        end
                    end
                    else if (inv_op == 5'd4) begin
                        if (!tlb_g[i] && (tlb_asid[i] == inv_asid)) begin
                            tlb_e[i] <= 1'b0;
                        end
                    end
                    else if (inv_op == 5'd5) begin
                        if (!tlb_g[i] && (tlb_asid[i] == inv_asid) && 
                           ((tlb_ps[i] == 6'd12) ? (tlb_vppn[i] == inv_vpn) : (tlb_vppn[i][18:10] == inv_vpn[18:10]))) begin
                            tlb_e[i] <= 1'b0;
                        end
                    end
                    else if (inv_op == 5'd6) begin
                        if ((tlb_g[i] || (tlb_asid[i] == inv_asid)) && 
                           ((tlb_ps[i] == 6'd12) ? (tlb_vppn[i] == inv_vpn) : (tlb_vppn[i][18:10] == inv_vpn[18:10]))) begin
                            tlb_e[i] <= 1'b0;
                        end
                    end
                end
            end
        end 
endgenerate

endmodule
