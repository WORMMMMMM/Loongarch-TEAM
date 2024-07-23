`include "myCPU.v"

module regcsr(
    input  clk,
    input  reset,

    input  csr_we,
    input  [13:0] csr_num,
    input  [31:0] csr_wmask,
    input  [31:0] csr_wdata,
    output [31:0] csr_rdata,

    input  excp_flush,
    input  ertn_flush,
    input  [ 5:0] ecode,
    input  [ 2:0] esubcode,
    input  [31:0] epc,

    output [31:0] era,
    output [31:0] eentry,

    input  [ 7:0] interrupt,
    output has_int
);

reg  [31:0] csr_crmd;
reg  [31:0] csr_prmd;
reg  [31:0] csr_ecfg;
reg  [31:0] csr_estat;
reg  [31:0] csr_era;
reg  [31:0] csr_badv;
reg  [31:0] csr_eentry;
reg  [31:0] csr_save0;
reg  [31:0] csr_save1;
reg  [31:0] csr_save2;
reg  [31:0] csr_save3;
reg  [31:0] csr_tid;
reg  [31:0] csr_tcfg;
reg  [31:0] csr_tval;
reg  [31:0] csr_ticlr;

wire csr_crmd_we;
wire csr_prmd_we;
wire csr_ecfg_we;
wire csr_estat_we;
wire csr_era_we;
wire csr_badv_we;
wire csr_eentry_we;
wire csr_save0_we;
wire csr_save1_we;
wire csr_save2_we;
wire csr_save3_we;
wire csr_tid_we;
wire csr_tcfg_we;
wire csr_ticlr_we;

wire next_tcfg;

wire hard_int;
wire soft_int;
wire ti_int;
wire ipi_int;

assign csr_crmd_we   = csr_we && csr_num == `CSR_CRMD;
assign csr_prmd_we   = csr_we && csr_num == `CSR_PRMD;
assign csr_ecfg_we   = csr_we && csr_num == `CSR_ECFG;
assign csr_estat_we  = csr_we && csr_num == `CSR_ESTAT;
assign csr_era_we    = csr_we && csr_num == `CSR_ERA;
assign csr_badv_we   = csr_we && csr_num == `CSR_BADV;
assign csr_eentry_we = csr_we && csr_num == `CSR_EENTRY;
assign csr_save0_we  = csr_we && csr_num == `CSR_SAVE0;
assign csr_save1_we  = csr_we && csr_num == `CSR_SAVE1;
assign csr_save2_we  = csr_we && csr_num == `CSR_SAVE2;
assign csr_save3_we  = csr_we && csr_num == `CSR_SAVE3;
assign csr_tid_we    = csr_we && csr_num == `CSR_TID;
assign csr_tcfg_we   = csr_we && csr_num == `CSR_TCFG;
assign csr_ticlr_we  = csr_we && csr_num == `CSR_TICLR;

always @(posedge clk) begin
    if (reset) begin
        csr_crmd[ `CSR_CRMD_PLV] <= 2'b0;
        csr_crmd[  `CSR_CRMD_IE] <= 1'b0;
        csr_crmd[  `CSR_CRMD_DA] <= 1'b1;
        csr_crmd[  `CSR_CRMD_PG] <= 1'b0;
        csr_crmd[`CSR_CRMD_DATF] <= 2'b0;
        csr_crmd[`CSR_CRMD_DATM] <= 2'b0;
        csr_crmd[31: 9] <= 23'b0;
    end
    else if (excp_flush) begin
        csr_crmd[`CSR_CRMD_PLV] <= 2'b0;
        csr_crmd[ `CSR_CRMD_IE] <= 1'b0;
    end
    else if (ertn_flush) begin
        csr_crmd[`CSR_CRMD_PLV] <= csr_prmd[`CSR_PRMD_PPLV];
        csr_crmd[ `CSR_CRMD_IE] <= csr_prmd[ `CSR_PRMD_PIE];
    end
    else if (csr_crmd_we) begin
        csr_crmd[ `CSR_CRMD_PLV] <= csr_wmask[ `CSR_CRMD_PLV] & csr_wdata[ `CSR_CRMD_PLV] | ~csr_wmask[ `CSR_CRMD_PLV] & csr_crmd[ `CSR_CRMD_PLV];
        csr_crmd[  `CSR_CRMD_IE] <= csr_wmask[  `CSR_CRMD_IE] & csr_wdata[  `CSR_CRMD_IE] | ~csr_wmask[  `CSR_CRMD_IE] & csr_crmd[  `CSR_CRMD_IE];
        csr_crmd[  `CSR_CRMD_DA] <= csr_wmask[  `CSR_CRMD_DA] & csr_wdata[  `CSR_CRMD_DA] | ~csr_wmask[  `CSR_CRMD_DA] & csr_crmd[  `CSR_CRMD_DA];
        csr_crmd[  `CSR_CRMD_PG] <= csr_wmask[  `CSR_CRMD_PG] & csr_wdata[  `CSR_CRMD_PG] | ~csr_wmask[  `CSR_CRMD_PG] & csr_crmd[  `CSR_CRMD_PG];
        csr_crmd[`CSR_CRMD_DATF] <= csr_wmask[`CSR_CRMD_DATF] & csr_wdata[`CSR_CRMD_DATF] | ~csr_wmask[`CSR_CRMD_DATF] & csr_crmd[`CSR_CRMD_DATF];
        csr_crmd[`CSR_CRMD_DATM] <= csr_wmask[`CSR_CRMD_DATM] & csr_wdata[`CSR_CRMD_DATM] | ~csr_wmask[`CSR_CRMD_DATM] & csr_crmd[`CSR_CRMD_DATM];
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_prmd[31: 3] <= 29'b0;
    end
    else if (excp_flush) begin
        csr_prmd[`CSR_PRMD_PPLV] <= csr_crmd[`CSR_CRMD_PLV];
        csr_prmd[ `CSR_PRMD_PIE] <= csr_crmd[ `CSR_CRMD_IE];
    end
    else if (csr_prmd_we) begin
        csr_prmd[`CSR_PRMD_PPLV] <= csr_wmask[`CSR_PRMD_PPLV] & csr_wdata[`CSR_PRMD_PPLV] | ~csr_wmask[`CSR_PRMD_PPLV] & csr_prmd[`CSR_PRMD_PPLV];
        csr_prmd[ `CSR_PRMD_PIE] <= csr_wmask[ `CSR_PRMD_PIE] & csr_wdata[ `CSR_PRMD_PIE] | ~csr_wmask[ `CSR_PRMD_PIE] & csr_prmd[ `CSR_PRMD_PIE];
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_ecfg <= 32'b0;
    end
    else if (csr_ecfg_we) begin
        csr_ecfg[ 9: 0] <= csr_wmask[ 9: 0] & csr_wdata[ 9: 0] | ~csr_wmask[ 9: 0] & csr_ecfg[ 9: 0];
        csr_ecfg[12:11] <= csr_wmask[12:11] & csr_wdata[12:11] | ~csr_wmask[12:11] & csr_ecfg[12:11];
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_estat[ 1: 0] <= 2'b0;
        csr_estat[10:10] <= 1'b0;
        csr_estat[15:13] <= 3'b0;
        csr_estat[31:31] <= 1'b0;
    end
    else begin
        csr_estat[ 9: 2] <= interrupt;
        if (excp_flush) begin
            csr_estat[`CSR_ESTAT_ECODE] <= ecode;
            csr_estat[`CSR_ESTAT_ESUBC] <= esubcode;
        end
        else if (csr_estat_we) begin
            csr_estat[ 1: 0] <= csr_Wmask[ 1: 0] & csr_wdata[ 1: 0] | ~csr_wmask[ 1: 0] & csr_estat[ 1: 0];
        end
    end
end

always @(posedge clk) begin
    if (excp_flush) begin
        csr_era <= epc;
    end
    else if (csr_era_we) begin
        csr_era <= csr_wmask & csr_wdata | ~csr_wmask & csr_era;
    end
end

always @(posedge clk) begin
    if (csr_badv_we) begin
        csr_badv <= csr_wmask & csr_wdata | ~csr_wmask & csr_badv;
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_eentry[ 5: 0] <= 6'b0;
    end
    else if (csr_eentry_we) begin
        csr_eentry[`CSR_EENTRY_VA] <= csr_wmask[`CSR_EENTRY_VA] & csr_wdata[`CSR_EENTRY_VA] | ~csr_wmask[`CSR_EENTRY_VA] & csr_eentry[`CSR_EENTRY_VA];
    end
end

always @(posedge clk) begin
    if (csr_save0_we) begin
        csr_save0 <= csr_wmask & csr_wdata | ~csr_wmask & csr_save0;
    end
    if (csr_save1_we) begin
        csr_save1 <= csr_wmask & csr_wdata | ~csr_wmask & csr_save1;
    end
    if (csr_save2_we) begin
        csr_save2 <= csr_wmask & csr_wdata | ~csr_wmask & csr_save2;
    end
    if (csr_save3_we) begin
        csr_save3 <= csr_wmask & csr_wdata | ~csr_wmask & csr_save3;
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_tid <= 32'b0;
    end
    else if (csr_tid_we) begin
        csr_tid <= csr_wmask & csr_wdata | ~csr_wmask & csr_tid;
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_tcfg[`CSR_TCFG_EN] <= 1'b0;
    end
    else if (csr_tcfg_we) begin
        csr_tcfg[   `CSR_TCFG_EN] <= csr_wmask[   `CSR_TCFG_EN] & csr_wdata[   `CSR_TCFG_EN] | ~csr_wmask[   `CSR_TCFG_EN] & csr_tcfg[   `CSR_TCFG_EN];
        csr_tcfg[  `CSR_TCFG_PER] <= csr_wmask[  `CSR_TCFG_PER] & csr_wdata[  `CSR_TCFG_PER] | ~csr_wmask[  `CSR_TCFG_PER] & csr_tcfg[  `CSR_TCFG_PER];
        csr_tcfg[`CSR_TCFG_INITV] <= csr_wmask[`CSR_TCFG_INITV] & csr_wdata[`CSR_TCFG_INITV] | ~csr_wmask[`CSR_TCFG_INITV] & csr_tcfg[`CSR_TCFG_INITV];
    end
end

always @(posedge clk) begin
    if (csr_tcfg_we) begin
        csr_tval <= {next_tcfg[`CSR_TCFG_INITV], 2'b0};
    end
    else if (csr_tcfg[`CSR_TCFG_EN]) begin
        if (csr_tval == 32'b0 && csr_tcfg[`CSR_TCFG_PER]) begin
            csr_tval <= {csr_tcfg[`CSR_TCFG_INITV], 2'b0};
        end
        else if (csr_tval != 32'b0) begin
            csr_tval <= csr_tval - 32'b1;
        end
    end
end

always @(posedge clk) begin
    if (reset) begin
        csr_ticlr <= 32'b0;
    end
end

assign next_tcfg = csr_wmask & csr_wdata | ~csr_wmask & csr_tcfg;

assign csr_rdata = {32{csr_num == `CSR_CRMD  }} & csr_crmd
                 | {32{csr_num == `CSR_PRMD  }} & csr_prmd
                 | {32{csr_num == `CSR_ECFG  }} & csr_ecfg
                 | {32{csr_num == `CSR_ESTAT }} & csr_estat
                 | {32{csr_num == `CSR_ERA   }} & csr_era
                 | {32{csr_num == `CSR_BADV  }} & csr_badv
                 | {32{csr_num == `CSR_EENTRY}} & csr_eentry
                 | {32{csr_num == `CSR_SAVE0 }} & csr_save0
                 | {32{csr_num == `CSR_SAVE1 }} & csr_save1
                 | {32{csr_num == `CSR_SAVE2 }} & csr_save2
                 | {32{csr_num == `CSR_SAVE3 }} & csr_save3;

assign era    = csr_era;
assign eentry = csr_eentry;

assign soft_int = | (csr_estat[ 1: 0] & csr_ecfg[ 1: 0]);
assign hard_int = | (csr_estat[ 9: 2] & csr_ecfg[ 9: 2]);
assign ti_int   = | (csr_estat[11:11] & csr_ecfg[11:11]);
assign ipi_int  = | (csr_estat[12:12] & csr_ecfg[12:12]);
assign has_int  = csr_crmd[`CSR_CRMD_IE] & (soft_int | hard_int | ti_int | ipi_int);

endmodule