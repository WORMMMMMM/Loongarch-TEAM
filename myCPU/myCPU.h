`ifndef myCPU_H
    `define myCPU_H

    /* bus */
    `define FS_TO_DS_BUS_WD 81
    `define DS_TO_ES_BUS_WD 268
    `define ES_TO_MS_BUS_WD 217
    `define MS_TO_WS_BUS_WD 168

    `define BR_BUS_WD 33
    `define WB_BUS_WD 38

    /* forward */
    `define ES_FORWARD_WD 80
    `define MS_FORWARD_WD 80
    `define WS_FORWARD_WD 80

    /* exception */
    `define ECODE_INT 6'h00
    `define ECODE_PIL 6'h01
    `define ECODE_PIS 6'h02
    `define ECODE_PIF 6'h03
    `define ECODE_PME 6'h04
    `define ECODE_PPI 6'h07
    `define ECODE_ADE 6'h08
    `define ECODE_ALE 6'h09
    `define ECODE_SYS 6'h0b
    `define ECODE_BRK 6'h0c
    `define ECODE_INE 6'h0d
    `define ECODE_IPE 6'h0e
    `define ECODE_TLBR 6'h3f

    /* regcsr */
    `define CSR_CRMD   14'h000
    `define CSR_PRMD   14'h001
    `define CSR_ECFG   14'h004
    `define CSR_ESTAT  14'h005
    `define CSR_ERA    14'h006
    `define CSR_BADV   14'h007
    `define CSR_EENTRY 14'h00c
    `define CSR_SAVE0  14'h030
    `define CSR_SAVE1  14'h031
    `define CSR_SAVE2  14'h032
    `define CSR_SAVE3  14'h033
    `define CSR_TID    14'h040
    `define CSR_TCFG   14'h041
    `define CSR_TVAL   14'h042
    `define CSR_TICLR  14'h044

    `define CSR_CRMD_PLV    1:0
    `define CSR_CRMD_IE     2
    `define CSR_CRMD_DA     3
    `define CSR_CRMD_PG     4
    `define CSR_CRMD_DATF   6:5
    `define CSR_CRMD_DATM   8:7
    `define CSR_PRMD_PPLV   1:0
    `define CSR_PRMD_PIE    2
    `define CSR_ESTAT_ECODE 21:16
    `define CSR_ESTAT_ESUBC 30:22
    `define CSR_EENTRY_VA   31:6
    `define CSR_TCFG_EN     0
    `define CSR_TCFG_PER    1
    `define CSR_TCFG_INITV  31:2

`endif
