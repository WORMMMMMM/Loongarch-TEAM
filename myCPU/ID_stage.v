`include "myCPU.h"

module id_stage(
    input  clk,
    input  reset,

    // allowin
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
    input  [`ES_FORWARD_WD-1:0] es_forward,
    input  [`MS_FORWARD_WD-1:0] ms_forward,
    input  [`WS_FORWARD_WD-1:0] ws_forward,

    input  excp_flush,
    input  ertn_flush,
    input  has_int
);

/* handshaking */
wire ds_flush;
wire ds_stall;
wire ds_ready_go_r;
wire ds_ready_go;
reg  ds_valid;
reg  [`FS_TO_DS_BUS_WD-1:0] fs_to_ds_bus_r;

/* decode */
wire [31:0] pc;
wire [31:0] inst;

wire [ 5:0] op_31_26;
wire [ 3:0] op_25_22;
wire [ 1:0] op_21_20;
wire [ 4:0] op_19_15;
wire [ 4:0] rk;
wire [ 4:0] rj;
wire [ 4:0] rd;
wire [ 4:0] i5;
wire [11:0] i12;
wire [15:0] i16;
wire [19:0] i20;
wire [25:0] i26;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;

/* instruction */
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

wire inst_rdcntid;
wire inst_rdcntvl_w;
wire inst_rdcntvh_w;
wire inst_break;
wire inst_syscall;
wire inst_csrrd;
wire inst_csrwr;
wire inst_csrxchg;
wire inst_ertn;

/* imm */
wire need_ui5;
wire need_ui12;
wire need_si12;
wire need_si20;
wire need_offs16;
wire need_offs26;
wire [31:0] imm;

/* regfile */
wire raddr1_op;
wire raddr2_op;
wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;
wire rf_we;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;
wire [31:0] rj_value;
wire [31:0] rkd_value;

/* alu */
wire src1_is_pc;
wire src2_is_imm;
wire src2_is_4;
wire [13:0] alu_op;
wire [ 3:0] mul_div_op;
wire mul_div_sign;

/* mem */
wire mem_en;
wire mem_we;

/* write back */
wire [ 4:0] dest;
wire ds_rf_we;
wire res_from_cnt;
wire res_from_mem;
wire res_from_csr;

/* branch */
wire sign;
wire [31:0] result;
wire overflow;
wire rj_eq_rd;
wire rj_lt_rd;
wire rj_ltu_rd;
wire br_taken;
wire [31:0] br_target;

/* exception */
wire excp_int;
wire excp_sys;
wire excp_brk;
wire excp_ine;
wire fs_excp;
wire ds_excp;
wire [15:0] fs_excp_num;
wire [15:0] ds_excp_num;

/* csr */
wire ds_csr_we;
wire [13:0] csr_num;
wire [31:0] csr_wmask;
wire [31:0] csr_wdata;

/* hazard */
wire es_valid;
wire es_rf_we;
wire [ 4:0] es_dest;
wire [31:0] es_final_result;
wire es_res_from_mem;
wire es_res_from_csr;

wire ms_valid;
wire ms_rf_we;
wire [ 4:0] ms_dest;
wire [31:0] ms_final_result;
wire ms_res_from_mem;
wire ms_res_from_csr;
wire ms_sram_data_ok;

wire ws_valid;
wire ws_rf_we;
wire [ 4:0] ws_dest;
wire [31:0] ws_final_result;

wire no_src1;
wire no_src2;
wire [ 4:0] src1;
wire [ 4:0] src2;
wire raw_ed_1_r;
wire raw_ed_2_r;
wire raw_md_1_r;
wire raw_md_2_r;
wire raw_wd_1_r;
wire raw_wd_2_r;
wire raw_ed_1;
wire raw_ed_2;
wire raw_md_1;
wire raw_md_2;
wire raw_wd_1;
wire raw_wd_2;
wire raw;


assign ds_flush       = excp_flush || ertn_flush;
assign ds_stall       = es_valid && es_res_from_mem && (raw_ed_1 || raw_ed_2)                     // load-use stall
                     || ms_valid && ms_res_from_mem && (raw_md_1 || raw_md_2) && !ms_sram_data_ok // load-use stall
                     || es_valid && es_res_from_csr && (raw_ed_1 || raw_ed_2)  // csr-use stall
                     || ms_valid && ms_res_from_csr && (raw_md_1 || raw_md_2); // csr-use stall
assign ds_ready_go_r  = !ds_flush && !ds_stall;
assign ds_ready_go    = (ds_ready_go_r === 1'bx) ? 1'b1 : ds_ready_go_r;
assign ds_allowin     = !ds_valid || ds_ready_go && es_allowin || ds_flush;
assign ds_to_es_valid = ds_valid && ds_ready_go;
always @(posedge clk) begin
    if (reset) begin
        ds_valid <= 1'b0;
    end
    else if (ds_allowin) begin
        ds_valid <= fs_to_ds_valid;
    end

    if (fs_to_ds_valid && ds_allowin) begin
        fs_to_ds_bus_r <= fs_to_ds_bus;
    end
end

assign {
    pc,
    inst,
    fs_excp,
    fs_excp_num
} = fs_to_ds_bus_r;

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

assign inst_rdcntid   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & (rk == 5'h18) & (rd == 5'h00);
assign inst_rdcntvl_w = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & (rk == 5'h18) & (rj == 5'h00);
assign inst_rdcntvh_w = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h0] & op_19_15_d[5'h00] & (rk == 5'h19) & (rj == 5'h00);
assign inst_break     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h14];
assign inst_syscall   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h16];
assign inst_csrrd     = op_31_26_d[6'h01] & ~inst[25] & ~inst[24] & (rj == 5'b0);
assign inst_csrwr     = op_31_26_d[6'h01] & ~inst[25] & ~inst[24] & (rj == 5'b1);
assign inst_csrxchg   = op_31_26_d[6'h01] & ~inst[25] & ~inst[24] & ~inst_csrrd & ~inst_csrwr;
assign inst_ertn      = op_31_26_d[6'h01] & op_25_22_d[4'h9] & op_21_20_d[2'h0] & op_19_15_d[5'h10] & (rk == 5'h0e) & (rj == 5'h00) & (rd == 5'h00);

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

assign raddr1_op = inst_lu12i_w | inst_pcaddu12i;
assign raddr2_op = inst_br | inst_st_w | inst_st_b | inst_st_h | inst_csrrd | inst_csrwr | inst_csrxchg;
assign rf_raddr1 = raddr1_op ? 5'b0 : rj;
assign rf_raddr2 = raddr2_op ? rd : rk;

assign {
    rf_we,
    rf_waddr,
    rf_wdata
} = wb_bus;

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

assign src1_is_pc  = inst_bl | inst_jirl | inst_pcaddu12i;
assign src2_is_imm = inst_slli_w | inst_srli_w | inst_srai_w | inst_slti | inst_sltui | inst_addi_w | inst_andi | inst_ori | inst_xori
                   | inst_ld_b | inst_ld_h | inst_ld_w | inst_st_b | inst_st_h | inst_st_w | inst_ld_bu | inst_ld_hu
                   | inst_lu12i_w | inst_pcaddu12i;
assign src2_is_4   = inst_bl | inst_jirl;

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
assign mul_div_op[0] = inst_mul_w;
assign mul_div_op[1] = inst_mulh_w | inst_mulh_wu;
assign mul_div_op[2] = inst_div_w  | inst_div_wu;
assign mul_div_op[3] = inst_mod_w  | inst_mod_wu;
assign mul_div_sign  = inst_mul_w | inst_mulh_w | inst_div_w | inst_mod_w;

assign mem_en = inst_mem;
assign mem_we = inst_st_b | inst_st_h | inst_st_w;

assign dest  = inst_bl      ? 5'h01 :
               inst_rdcntid ? rj:
               rd;
assign ds_rf_we = ~inst_st_w & ~inst_st_b & ~inst_st_h & ~inst_b & ~inst_br & ~inst_syscall & ~inst_ertn;
assign res_from_cnt = inst_rdcntvl_w | inst_rdcntvh_w;
assign res_from_mem = inst_ld_b | inst_ld_h | inst_ld_w | inst_ld_bu | inst_ld_hu;
assign res_from_csr = inst_rdcntid | inst_csrrd | inst_csrwr | inst_csrxchg;

assign {sign, result} = {1'b0, rj_value} + {1'b1, ~rkd_value} + 33'd1;
assign overflow = (rj_value[31] ^ rkd_value[31]) & (rj_value[31] ^ result[31]);
assign rj_eq_rd  = (rj_value == rkd_value);
assign rj_lt_rd  = result[31] ^ overflow;
assign rj_ltu_rd = sign;
assign br_taken  = !ds_stall && ds_valid && (inst_jirl | inst_b | inst_bl
                              | inst_beq  &  rj_eq_rd
                              | inst_bne  & ~rj_eq_rd
                              | inst_blt  &  rj_lt_rd
                              | inst_bge  & ~rj_lt_rd
                              | inst_bltu &  rj_ltu_rd
                              | inst_bgeu & ~rj_ltu_rd);
assign br_target = inst_jirl ? rj_value + imm : pc + imm;

assign br_bus = {
    br_taken,
    br_target
};

assign excp_int = has_int;
assign excp_sys = inst_syscall;
assign excp_brk = inst_break;
assign excp_ine = ~inst_add_w & ~inst_sub_w & ~inst_slt & ~inst_sltu & ~inst_nor & ~inst_and & ~inst_or & ~inst_xor & ~inst_sll_w & ~inst_srl_w & ~inst_sra_w
                & ~inst_mul_w & ~inst_mulh_w & ~inst_mulh_wu & ~inst_div_w & ~inst_mod_w & ~inst_div_wu & ~inst_mod_wu
                & ~inst_slli_w & ~inst_srli_w & ~inst_srai_w & ~inst_slti & ~inst_sltui & ~inst_addi_w & ~inst_andi & ~inst_ori & ~inst_xori
                & ~inst_ld_b & ~inst_ld_h & ~inst_ld_w & ~inst_st_b & ~inst_st_h & ~inst_st_w & ~inst_ld_bu & ~inst_ld_hu
                & ~inst_jirl & ~inst_b & ~inst_bl & ~inst_beq & ~inst_bne & ~inst_blt & ~inst_bge & ~inst_bltu & ~inst_bgeu & ~inst_lu12i_w & ~inst_pcaddu12i
                & ~inst_rdcntid & ~inst_rdcntvl_w & ~inst_rdcntvh_w & ~inst_break & ~inst_syscall & ~inst_csrrd & ~inst_csrwr & ~inst_csrxchg & ~inst_ertn;
assign ds_excp     = fs_excp | excp_int | excp_sys | excp_brk | excp_ine;
assign ds_excp_num = fs_excp_num | {excp_int, 4'b0, excp_sys, excp_brk, excp_ine, 8'b0};

assign ds_csr_we    = inst_csrwr | inst_csrxchg;
assign csr_num   = inst_rdcntid ? `CSR_TID : inst[23:10];
assign csr_wmask = inst_csrxchg ? rj_value : 32'hffffffff;
assign csr_wdata = rkd_value;

assign {
    es_valid,
    es_rf_we,
    es_dest,
    es_final_result,
    es_res_from_mem,
    es_res_from_csr
} = es_forward;

assign {
    ms_valid,
    ms_rf_we,
    ms_dest,
    ms_final_result,
    ms_res_from_mem,
    ms_res_from_csr,
    ms_sram_data_ok
} = ms_forward;

assign {
    ws_valid,
    ws_rf_we,
    ws_dest,
    ws_final_result
} = ws_forward;

// RAW hazard
assign no_src1 = inst_b | inst_bl | inst_pcaddu12i | inst_rdcntvl_w | inst_rdcntvh_w | inst_rdcntid | inst_csrrd | inst_csrwr;
assign no_src2 = inst_slli_w | inst_srli_w | inst_srai_w | inst_slti | inst_sltui | inst_addi_w | inst_andi | inst_ori | inst_xori
               | inst_ld_w | inst_ld_b | inst_ld_bu | inst_ld_h | inst_ld_hu
               | inst_b | inst_bl | inst_jirl | inst_pcaddu12i | inst_rdcntvh_w | inst_rdcntid | inst_csrrd;

assign src1 = no_src1 ? 5'd0 : rf_raddr1;
assign src2 = no_src2 ? 5'd0 : rf_raddr2;

assign raw_ed_1_r = es_valid && (src1 == es_dest) && (src1 != 5'h0) && es_rf_we;
assign raw_ed_2_r = es_valid && (src2 == es_dest) && (src2 != 5'h0) && es_rf_we;
assign raw_md_1_r = ms_valid && (src1 == ms_dest) && (src1 != 5'h0) && ms_rf_we;
assign raw_md_2_r = ms_valid && (src2 == ms_dest) && (src2 != 5'h0) && ms_rf_we;
assign raw_wd_1_r = ws_valid && (src1 == ws_dest) && (src1 != 5'h0) && ws_rf_we;
assign raw_wd_2_r = ws_valid && (src2 == ws_dest) && (src2 != 5'h0) && ws_rf_we;

assign raw_ed_1 = (raw_ed_1_r === 1'bx) ? 1'b0 : raw_ed_1_r;
assign raw_ed_2 = (raw_ed_2_r === 1'bx) ? 1'b0 : raw_ed_2_r;
assign raw_md_1 = (raw_md_1_r === 1'bx) ? 1'b0 : raw_md_1_r;
assign raw_md_2 = (raw_md_2_r === 1'bx) ? 1'b0 : raw_md_2_r;
assign raw_wd_1 = (raw_wd_1_r === 1'bx) ? 1'b0 : raw_wd_1_r;
assign raw_wd_2 = (raw_wd_2_r === 1'bx) ? 1'b0 : raw_wd_2_r;

assign raw = raw_ed_1 | raw_ed_2 | raw_md_1 | raw_md_2 | raw_wd_1 | raw_wd_2;

assign rj_value  = raw_ed_1 ? es_final_result :
                   raw_md_1 ? ms_final_result :
                   raw_wd_1 ? ws_final_result : rf_rdata1;
assign rkd_value = raw_ed_2 ? es_final_result :
                   raw_md_2 ? ms_final_result :
                   raw_wd_2 ? ws_final_result : rf_rdata2;

assign ds_to_es_bus = {
    pc,
    inst_ld_b,
    inst_ld_h,
    inst_ld_w,
    inst_st_b,
    inst_st_h,
    inst_st_w,
    inst_ld_bu,
    inst_ld_hu,
    inst_rdcntvl_w,
    inst_rdcntvh_w,
    inst_ertn,
    imm,
    rj_value,
    rkd_value,
    src1_is_pc,
    src2_is_imm,
    src2_is_4,
    alu_op,
    mem_en,
    mem_we,
    dest,
    ds_rf_we,
    res_from_cnt,
    res_from_mem,
    res_from_csr,
    mul_div_op,
    mul_div_sign,
    ds_excp,
    ds_excp_num,
    ds_csr_we,
    csr_num,
    csr_wmask,
    csr_wdata,
    inst
};

endmodule