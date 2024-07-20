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
    input  [ 5: 0] ecode,
    input  [ 2: 0] esubcode,
    input  [31: 0] epc,

    output [31: 0] era,
    output [31: 0] eentry,
);

reg  [31:0] csr_crmd;
reg  [31:0] csr_prmd;
reg  [31:0] csr_estat;
reg  [31:0] csr_era;
reg  [31:0] csr_eentry;
reg  [31:0] csr_save0;
reg  [31:0] csr_save1;
reg  [31:0] csr_save2;
reg  [31:0] csr_save3;

wire csr_crmd_we;
wire csr_prmd_we;
wire csr_estat_we;
wire csr_era_we;
wire csr_eentry_we;
wire csr_save0_we;
wire csr_save1_we;
wire csr_save2_we;
wire csr_save3_we;

assign csr_crmd_we   = csr_we && csr_num == `CSR_CRMD;
assign csr_prmd_we   = csr_we && csr_num == `CSR_PRMD;
assign csr_estat_we  = csr_we && csr_num == `CSR_ESTAT;
assign csr_erat_we   = csr_we && csr_num == `CSR_ERA;
assign csr_eentry_we = csr_we && csr_num == `CSR_EENTRY;
assign csr_save0_we  = csr_we && csr_num == `CSR_SAVE0;
assign csr_save1_we  = csr_we && csr_num == `CSR_SAVE1;
assign csr_save2_we  = csr_we && csr_num == `CSR_SAVE2;
assign csr_save3_we  = csr_we && csr_num == `CSR_SAVE3;

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
        csr_estat[ 1: 0] <= 2'b0;
        csr_estat[10:10] <= 1'b0;
        csr_estat[15:13] <= 3'b0;
        csr_estat[31:31] <= 1'b0;
    end
    else if (excp_flush) begin
        csr_estat[`CSR_ESTAT_ECODE] <= ecode;
        csr_estat[`CSR_ESTAT_ESUBC] <= esubcode;
    end
    else if (csr_estat_we) begin
        csr_estat[ 1: 0] <= csr_Wmask[ 1: 0] & csr_wdata[ 1: 0] | ~csr_wmask[ 1: 0] & csr_estat[ 1: 0];
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

assign csr_rdata = {32{csr_num == `CSR_CRMD  }} & csr_crmd
                 | {32{csr_num == `CSR_PRMD  }} & csr_prmd
                 | {32{csr_num == `CSR_ESTAT }} & csr_estat
                 | {32{csr_num == `CSR_ERA   }} & csr_era
                 | {32{csr_num == `CSR_EENTRY}} & csr_eentry
                 | {32{csr_num == `CSR_SAVE0 }} & csr_save0
                 | {32{csr_num == `CSR_SAVE1 }} & csr_save1
                 | {32{csr_num == `CSR_SAVE2 }} & csr_save2
                 | {32{csr_num == `CSR_SAVE3 }} & csr_save3;

assign era    = csr_era;
assign eentry = csr_eentry;

endmodule