`include "myCPU.h"

module id_stage(
    input  clk,
    input  reset,

    input  es_allowin,
    output ds_allowin,

    // from fs
    input  fs_to_ds_valid,
    input  [`FS_TO_DS_BUS_WD-1:0] fs_to_ds_bus,

    // to es
    output ds_to_es_valid,
    output [`DS_TO_ES_BUS_WD-1:0] ds_to_es_bus,
    
    output [`BR_BUS_WD-1:0] br_bus,
    input  [`WB_BUS_WD-1:0] wb_bus,
    input  [`ES_FORWARD_WD   -1:0] es_forward,
    input  [`MS_FORWARD_WD   -1:0] ms_forward,
    input  [`WS_FORWARD_WD   -1:0] ws_forward
);

/* handshaking */

reg  [`FS_TO_DS_BUS_WD-1:0] fs_to_ds_bus_r;

/* decode */
wire [31: 0] inst;
wire [31: 0] pc;

wire [ 5: 0] op_31_26;
wire [ 3: 0] op_25_22;
wire [ 1: 0] op_21_20;
wire [ 4: 0] op_19_15;
wire [ 4: 0] rk;
wire [ 4: 0] rj;
wire [ 4: 0] rd;
wire [ 4: 0] i5;
wire [11: 0] i12;
wire [15: 0] i16;
wire [19: 0] i20;
wire [25: 0] i26;

wire [63: 0] op_31_26_d;
wire [15: 0] op_25_22_d;
wire [ 3: 0] op_21_20_d;
wire [31: 0] op_19_15_d;

/* instruction */
wire inst_rdcntid;
wire inst_rdcntvl_w;
wire inst_rdcntvh_w;
wire inst_add_w;
wire inst_sub_w;
wire inst_slt;
wire inst_sltu;
wire inst_nor;
wire inst_and;
wire inst_andn;
wire inst_or;
wire inst_xor;
wire inst_sll_w;
wire inst_srl_w;
wire inst_sra_w;
wire inst_mul_w;
wire inst_mulh_w;
wire inst_mulh_wu;
wire inst_div_w;
wire inst_mod_w;
wire inst_div_wu;
wire inst_mod_wu;
wire inst_break;
wire inst_syscall;
wire inst_slli_w;
wire inst_srli_w;
wire inst_srai_w;
wire inst_slti;
wire inst_sltui;
wire inst_addi_w;
wire inst_andi;
wire inst_ori;
wire inst_xori;
wire inst_orn;
wire inst_ld_b;
wire inst_ld_h;
wire inst_ld_w;
wire inst_st_b;
wire inst_st_h;
wire inst_st_w;
wire inst_ld_bu;
wire inst_ld_hu;
wire inst_jirl;
wire inst_b;
wire inst_bl;
wire inst_beq;
wire inst_bne;
wire inst_blt;
wire inst_bge;
wire inst_bltu;
wire inst_bgeu;
wire inst_lu12i_w;
wire inst_pcaddu12i;

wire inst_mem;
wire inst_br;

/* imm */
wire need_ui5;
wire need_ui12;
wire need_si12;
wire need_si20;
wire need_offs16;
wire need_offs26;
wire [31: 0] imm;

/* regfile */
wire raddr1_op;
wire [ 4: 0] rf_raddr1;
wire [31: 0] rf_rdata1;
wire [ 4: 0] rf_raddr2;
wire [31: 0] rf_rdata2;
wire rf_we;
wire [ 4: 0] rf_waddr;
wire [31: 0] rf_wdata;
wire [31: 0] rkd_value;
wire [31: 0] rj_value;

/* alu */
wire src1_is_pc;
wire src2_is_imm;
wire src2_is_4;
wire [18: 0] alu_op;
wire [ 3: 0] mul_div_op;
wire mul_div_sign;
/* mem */
wire mem_e;
wire mem_we;
wire load_block;
/* write back */
wire [ 4: 0] wb_dest;
wire wb_gr_we;
wire wb_src_op; // 0: alu_result, 1: mem_data

/* branch */
wire sign;
wire [31: 0] result;
wire overflow;
wire rj_eq_rd;
wire rj_lt_rd;
wire rj_ltu_rd;
wire br_taken;
wire br_stall;
wire [31: 0] br_target;
/*----------------- Handshaking-----------------*/
assign  load_block = es_valid && es_inst_load && (raw_ed_1_r || raw_ed_2_r) ||
                     ms_valid && ms_res_from_mem && (raw_md_1_r || raw_md_2_r) && ~ms_data_sram_data_ok;
wire ds_ready_go;
wire ds_ready_go_r;
reg  ds_valid;
assign ds_ready_go = !load_block;// last inst is load or the former inst is load and data hasn't been received
assign ds_allowin     = ~ds_valid | ds_ready_go & es_allowin;
assign ds_to_es_valid =  ds_valid & ds_ready_go;
always @(posedge clk) begin
    if (reset)
        ds_valid <= 1'b0;
    else if (br_taken)    //////////////
        ds_valid <= 1'b0;
    else if (ds_allowin)
        ds_valid <= fs_to_ds_valid;
    if (fs_to_ds_valid && ds_allowin)
        fs_to_ds_bus_r <= fs_to_ds_bus;
end
/*----------------- Decode-----------------*/
assign op_31_26 = inst[31:26];
assign op_25_22 = inst[25:22];
assign op_21_20 = inst[21:20];
assign op_19_15 = inst[19:15];
assign rk = inst[14:10];
assign rj = inst[ 9: 5];
assign rd = inst[ 4: 0];
assign i5  = inst[14:10];
assign i12 = inst[21:10];
assign i16 = inst[25:10];
assign i20 = inst[24: 5];
assign i26 = {inst[ 9: 0], inst[25:10]};

decoder_6_64 u_dec0(.in(op_31_26), .out(op_31_26_d));
decoder_4_16 u_dec1(.in(op_25_22), .out(op_25_22_d));
decoder_2_4  u_dec2(.in(op_21_20), .out(op_21_20_d));
decoder_5_32 u_dec3(.in(op_19_15), .out(op_19_15_d));

assign inst_rdcntid   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & (rk == 5'h18) & (rd == 5'h00);
assign inst_rdcntvl_w = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & (rk == 5'h18) & (rj == 5'h00);
assign inst_rdcntvh_w = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & (rk == 5'h19) & (rj == 5'h00);
assign inst_add_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h00];
assign inst_sub_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h02];
assign inst_slt       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h04];
assign inst_sltu      = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h05];
assign inst_nor       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h08];
assign inst_and       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h09];
assign inst_or        = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0a];
assign inst_xor       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0b];
assign inst_sll_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0e];
assign inst_srl_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0f];
assign inst_sra_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h10];
assign inst_mul_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h18];
assign inst_mulh_w    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h19];
assign inst_mulh_wu   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h1a];
assign inst_div_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h00];
assign inst_mod_w     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h01];
assign inst_div_wu    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h02];
assign inst_mod_wu    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h03];
assign inst_break     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h14];
assign inst_syscall   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h16];
assign inst_slli_w    = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h01];
assign inst_srli_w    = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h09];
assign inst_srai_w    = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h11];
assign inst_slti      = op_31_26_d[6'h00] & op_25_22_d[4'h8];
assign inst_sltui     = op_31_26_d[6'h00] & op_25_22_d[4'h9];
assign inst_addi_w    = op_31_26_d[6'h00] & op_25_22_d[4'ha];
assign inst_andi      = op_31_26_d[6'h00] & op_25_22_d[4'hd];
assign inst_ori       = op_31_26_d[6'h00] & op_25_22_d[4'he];
assign inst_xori      = op_31_26_d[6'h00] & op_25_22_d[4'hf];
assign inst_ld_b      = op_31_26_d[6'h0a] & op_25_22_d[4'h0];
assign inst_ld_h      = op_31_26_d[6'h0a] & op_25_22_d[4'h1];
assign inst_ld_w      = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_st_b      = op_31_26_d[6'h0a] & op_25_22_d[4'h4];
assign inst_st_h      = op_31_26_d[6'h0a] & op_25_22_d[4'h5];
assign inst_st_w      = op_31_26_d[6'h0a] & op_25_22_d[4'h6];
assign inst_ld_bu     = op_31_26_d[6'h0a] & op_25_22_d[4'h8];
assign inst_ld_hu     = op_31_26_d[6'h0a] & op_25_22_d[4'h9];
assign inst_jirl      = op_31_26_d[6'h13];
assign inst_b         = op_31_26_d[6'h14];
assign inst_bl        = op_31_26_d[6'h15];
assign inst_beq       = op_31_26_d[6'h16];
assign inst_bne       = op_31_26_d[6'h17];
assign inst_blt       = op_31_26_d[6'h18];
assign inst_bge       = op_31_26_d[6'h19];
assign inst_bltu      = op_31_26_d[6'h1a];
assign inst_bgeu      = op_31_26_d[6'h1b];
assign inst_lu12i_w   = op_31_26_d[6'h05] & ~inst[25];
assign inst_pcaddu12i = op_31_26_d[6'h07] & ~inst[25];
assign inst_andn      = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0d];
assign inst_orn       = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0c];

assign inst_mem       = inst_ld_b | inst_ld_h | inst_ld_w | inst_st_b | inst_st_h | inst_st_w | inst_ld_bu | inst_ld_hu;
assign inst_br        = inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu;

assign need_ui5    = inst_slli_w | inst_srli_w | inst_srai_w;
assign need_ui12   = inst_andi | inst_ori | inst_xori;
assign need_si12   = inst_slti | inst_sltui | inst_addi_w | inst_ld_b | inst_ld_h | inst_ld_w | inst_st_b | inst_st_h | inst_st_w | inst_ld_bu | inst_ld_hu;
assign need_si20   = inst_lu12i_w | inst_pcaddu12i;
assign need_offs16 = inst_jirl | inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu;
assign need_offs26 = inst_b | inst_bl;

assign imm = need_ui5    ? {27'h0, i5}:
             need_ui12   ? {20'h0, i12} :
             need_si12   ? {{20{i12[11]}}, i12} :
             need_si20   ? {i20, 12'b0} :
             need_offs16 ? {{14{i16[15]}}, i16, 2'b00} :
             need_offs26 ? {{ 4{i26[25]}}, i26, 2'b00} :
             32'h00000000;
/*----------------- Regfile interface-----------------*/
assign raddr1_op = inst_br | inst_st_w | inst_st_b | inst_st_h;
assign rf_raddr1 = raddr1_op ? rd : rk;
assign rf_raddr2 = inst_lu12i_w | inst_pcaddu12i ? 5'b0 : rj;
assign rf_we     = wb_bus[37:37];
assign rf_waddr  = wb_bus[36:32];
assign rf_wdata  = wb_bus[31: 0];

regfile u_regfile(
    .clk    (clk      ),
    .raddr1 (rf_raddr1),
    .rdata1 (rf_rdata1),
    .raddr2 (rf_raddr2),
    .rdata2 (rf_rdata2),
    .we     (rf_we    ),
    .waddr  (rf_waddr ),
    .wdata  (rf_wdata )
);


assign src1_is_pc  = inst_bl | inst_jirl | inst_pcaddu12i ? 1'b1 : 1'b0;
assign src2_is_imm = inst_slli_w | inst_srli_w | inst_srai_w | inst_slti | inst_sltui | inst_addi_w | inst_andi | inst_ori | inst_xori | inst_ld_b | inst_ld_h | inst_ld_w | inst_st_b | inst_st_h | inst_st_w | inst_ld_bu | inst_ld_hu | inst_lu12i_w | inst_pcaddu12i;
assign src2_is_4   = inst_bl | inst_jirl ? 1'b1 : 1'b0;

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_mem | inst_jirl | inst_bl | inst_pcaddu12i;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt | inst_slti;
assign alu_op[ 3] = inst_sltu | inst_sltui;
assign alu_op[ 4] = inst_and | inst_andi;
assign alu_op[ 5] = inst_nor; 
assign alu_op[ 6] = inst_or | inst_ori;
assign alu_op[ 7] = inst_xor | inst_xori;
assign alu_op[ 8] = inst_slli_w | inst_sll_w;
assign alu_op[ 9] = inst_srli_w | inst_srl_w;
assign alu_op[10] = inst_srai_w | inst_sra_w;
assign alu_op[11] = inst_lu12i_w;
assign alu_op[12] = inst_andn;
assign alu_op[13] = inst_orn;

//div and mul
assign mul_div_op[ 0] = inst_mul_w;
assign mul_div_op[ 1] = inst_mulh_w | inst_mulh_wu;
assign mul_div_op[ 2] = inst_div_w  | inst_div_wu;
assign mul_div_op[ 3] = inst_mod_w  | inst_mod_wu;
assign mul_div_sign   = inst_mul_w | inst_mulh_w |inst_div_w | inst_mod_w;

assign mem_e     = inst_mem;
assign mem_we    = inst_st_b | inst_st_h | inst_st_w;

assign wb_dest   = inst_bl ? 5'h01 : rd;
assign wb_gr_we  = ~inst_st_w & ~inst_st_b & ~inst_st_h & ~inst_b & ~inst_br & (wb_dest != 5'b0);
assign wb_src_op = inst_ld_b | inst_ld_h | inst_ld_w | inst_ld_bu | inst_ld_hu;
/*----------------- Comparison -----------------*/
assign {sign, result} = {1'b0, rj_value} + {1'b1, ~rkd_value} + 33'd1;
assign overflow = (rj_value[31] ^ rkd_value[31]) & (rj_value[31] ^ result[31]);
assign rj_eq_rd  = (rj_value == rkd_value);
assign rj_lt_rd  = result[31] ^ overflow;
assign rj_ltu_rd = sign;
assign br_taken  = ( inst_jirl | inst_b | inst_bl
                 | (inst_beq  &  rj_eq_rd)
                 | (inst_bne  & ~rj_eq_rd)
                 | (inst_blt  &  rj_lt_rd)
                 | (inst_bge  & ~rj_lt_rd)
                 | (inst_bltu &  rj_ltu_rd)
                 | (inst_bgeu & ~rj_ltu_rd)
                 ) && ds_valid && ds_ready_go;  ///////
assign br_target = inst_jirl ? rj_value + imm : pc + imm;
assign br_stall  = load_block;
/*----------------- BrBUS to FS -----------------*/
assign br_bus[   33] = br_stall;
assign br_bus[32:32] = br_taken;
assign br_bus[31: 0] = br_target;
/*----------------- Signal interface -----------------*/
//DS to ES bus
assign ds_to_es_bus[171:171] = mul_div_sign;
assign ds_to_es_bus[170:167] = mul_div_op;
assign ds_to_es_bus[166:135] = pc;
assign ds_to_es_bus[134:134] = inst_ld_b;
assign ds_to_es_bus[133:133] = inst_ld_h;
assign ds_to_es_bus[132:132] = inst_ld_w;
assign ds_to_es_bus[131:131] = inst_st_b;
assign ds_to_es_bus[130:130] = inst_st_h;
assign ds_to_es_bus[129:129] = inst_st_w;
assign ds_to_es_bus[128:128] = inst_ld_bu;
assign ds_to_es_bus[127:127] = inst_ld_hu;
assign ds_to_es_bus[126: 95] = imm;
assign ds_to_es_bus[ 94: 63] = rkd_value;
assign ds_to_es_bus[ 62: 31] = rj_value;
assign ds_to_es_bus[ 30: 30] = src1_is_pc;
assign ds_to_es_bus[ 29: 29] = src2_is_imm;
assign ds_to_es_bus[ 28: 28] = src2_is_4;
assign ds_to_es_bus[ 27:  9] = alu_op;
assign ds_to_es_bus[  8:  8] = mem_e;
assign ds_to_es_bus[  7:  7] = mem_we;
assign ds_to_es_bus[  6:  2] = wb_dest;
assign ds_to_es_bus[  1:  1] = wb_gr_we;
assign ds_to_es_bus[  0:  0] = wb_src_op;
//FS to DS bus
assign inst = fs_to_ds_bus_r[63:32];
assign pc   = fs_to_ds_bus_r[31: 0];
/*-----------------Hazard detection-----------------*/
//signals from ES
wire ex_valid;
wire es_gr_we;
wire [ 4: 0] es_dest;
wire [31: 0] es_alu_result;
wire [31: 0] es_pc;
wire es_inst_load;
assign es_valid     = es_forward[0];
assign es_gr_we     = es_forward[1];
assign es_dest      = es_forward[6:2];
assign es_alu_result= es_forward[38:7];
assign es_pc        = es_forward[70:39];
assign es_inst_load = es_forward[71];
//signals from MS
wire ms_valid;
wire ms_gr_we;
wire [ 4: 0] ms_dest;
wire [31: 0] ms_final_result;
wire ms_res_from_mem;
wire [31: 0] ms_pc;
wire ms_data_sram_data_ok;
assign ms_valid        = ms_forward[0];
assign ms_gr_we        = ms_forward[1];
assign ms_dest         = ms_forward[6:2];
assign ms_final_result = ms_forward[38:7];
assign ms_res_from_mem = ms_forward[39];
assign ms_pc           = ms_forward[71:40];
assign ms_data_sram_data_ok = ms_forward[72];
//signals from WS
wire ws_valid;
wire ws_gr_we;
wire [ 4: 0] ws_dest;
wire [31: 0] ws_final_result;
wire [31: 0] ws_pc;

assign ws_valid        = ws_forward[0];
assign ws_gr_we        = ws_forward[1];
assign ws_dest         = ws_forward[6:2];
assign ws_final_result = ws_forward[38:7];
assign ws_pc           = ws_forward[70:39];

//RAW hazard
wire raw;
wire raw_ed_1;//ES to DS 的数据冒�??
wire raw_ed_2;
wire raw_md_1;
wire raw_md_2;
wire raw_wd_1;
wire raw_wd_2;
wire br_stall;

//assign br_stall = es_valid && es_res_from_mem && need_offs16 && (raw_ed_1_r || raw_ed_2_r)
               //|| need_offs16 && (raw_md_1_r || raw_md_2_r)); ??? uncertain to add this
               //last inst is load inst and this inst is branch


assign no_src1 = inst_b || inst_bl || inst_pcaddu12i || 
                 inst_rdcntvl_w || inst_rdcntvh_w || inst_rdcntid;

assign no_src2 = inst_b || inst_bl || inst_jirl || inst_addi_w ||
                 inst_ld_w || inst_ld_b || inst_ld_bu || inst_ld_h || inst_ld_hu ||
                 inst_slli_w || inst_srli_w || inst_srai_w || inst_slti || inst_sltui ||
                 inst_andi || inst_ori || inst_xori || inst_pcaddu12i || inst_rdcntvh_w || inst_rdcntid;

wire [4:0] src1;
wire [4:0] src2;
assign src1 = no_src1 ? 5'd0 : rf_raddr2;
assign src2 = no_src2 ? 5'd0 : rf_raddr1;

wire raw_ed_1_r;
wire raw_ed_2_r;
wire raw_md_1_r;
wire raw_md_2_r;
wire raw_wd_1_r;
wire raw_wd_2_r;
 
assign raw_ed_1_r = es_valid && (src1 == es_dest) && (src1!=5'h0)&&(es_dest!=5'h0) && es_gr_we;
assign raw_ed_2_r = es_valid && (src2 == es_dest) && (src2!=5'h0)&&(es_dest!=5'h0) && es_gr_we;
assign raw_md_1_r = ms_valid && (src1 == ms_dest) && (src1!=5'h0)&&(ms_dest!=5'h0) && ms_gr_we;
assign raw_md_2_r = ms_valid && (src2 == ms_dest) && (src2!=5'h0)&&(ms_dest!=5'h0) && ms_gr_we;
assign raw_wd_1_r = ws_valid && (src1 == ws_dest) && (src1!=5'h0)&&(ws_dest!=5'h0) && ws_gr_we;
assign raw_wd_2_r = ws_valid && (src2 == ws_dest) && (src2!=5'h0)&&(ws_dest!=5'h0) && ws_gr_we;

assign raw_ed_1 = (raw_ed_1_r === 1'bx) ? 1'b0 : raw_ed_1_r;
assign raw_ed_2 = (raw_ed_2_r === 1'bx) ? 1'b0 : raw_ed_2_r;
assign raw_md_1 = (raw_md_1_r === 1'bx) ? 1'b0 : raw_md_1_r;
assign raw_md_2 = (raw_md_2_r === 1'bx) ? 1'b0 : raw_md_2_r;
assign raw_wd_1 = (raw_wd_1_r === 1'bx) ? 1'b0 : raw_wd_1_r;
assign raw_wd_2 = (raw_wd_2_r === 1'bx) ? 1'b0 : raw_wd_2_r;

assign raw=raw_ed_1 | raw_ed_2 | raw_md_1 | raw_md_2 | raw_wd_1 | raw_wd_2;

assign rj_value= raw_ed_1 ? es_alu_result :
                 raw_md_1 ? ms_final_result :
                 raw_wd_1 ? ws_final_result : rf_rdata2;
assign rkd_value= raw_ed_2 ? es_alu_result :  
                 raw_md_2 ? ms_final_result :
                 raw_wd_2 ? ws_final_result : rf_rdata1;

endmodule