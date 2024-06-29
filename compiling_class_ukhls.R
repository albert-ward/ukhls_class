# Packages
library(tidyverse)

# Compiles from Wave 9
base_path_bhps <- "SET THIS TO LOCATION OF BHPS FILES"
base_path_ukhls <- "SET THIS TO LOCATION OF UKHLS FILES"

# Computing NS-SeC according to ONS methodology

# Read WAVE BA
# istrtdaty equals 1991 for the whole sample
file_path_ba <- file.path(base_path_bhps, "ba_indresp.dta")
ba_full_temphrp_ssec <- read_dta(file_path_ba) %>%
  dplyr::select(pidp, ba_jlnssec8_dv, ba_jbnssec8_dv, ba_jboff, ba_jbhas, ba_jbstat, ba_cjsbly)

# Create a new NS-SeC variable (8-class), based on conditions above
ba_full_temphrp_ssec <- mutate(ba_full_temphrp_ssec,
  ba_nssec8_new = case_when(
    ba_jbstat != 4 & ba_jbstat != 7 & ba_jbhas == 1 | (ba_jbhas == 2 & ba_jboff == 1) ~ ba_jbnssec8_dv, # Asked if R did paid work last week, or did no paid work last week but has a job that was away from. Includes data fed forward from last year. This is the only condition for this - covers all employed.
    ba_jbstat != 4 & ba_jbstat != 7 & ba_jbhas == 2 & ba_jboff == 2 & ba_cjsbly == 2 ~ ba_jlnssec8_dv, # Has no job but had job in the past less than one year ago. Assume asked for all in first wave of BHPS
    ba_jbstat == 7 ~ 98, # Full-time students
    ba_jbstat == 4 ~ ba_jlnssec8_dv, # Retired
    ba_jbhas == 2 & ba_jboff == 2 & ba_cjsbly == 1 ~ 97 # Never worked / Has no job but had job in the past AND been unemployed for more than a year. Previously had to divide this because of different conditions for empstend and jlend. Note never worked and LT unemployed are functionally the same category, so this is fine.
  )
)


# Read WAVE BB
file_path_bb <- file.path(base_path_bhps, "bb_indresp.dta")
bb_full_temphrp_ssec <- read_dta(file_path_bb) %>%
  dplyr::select(pidp, bb_jlnssec8_dv, bb_jbnssec8_dv, bb_jboff, bb_jbhas, bb_jbstat, bb_ivlyr, bb_cjsbly)

# Feed forward variable from wave before
ba_full_temphrp_ssec2 <- dplyr::select(ba_full_temphrp_ssec, pidp, ba_nssec8_new)
bb_full_temphrp_ssec <- left_join(bb_full_temphrp_ssec, ba_full_temphrp_ssec2, by = "pidp")

# Create a new NS-SeC variable (8-class), based on conditions above
bb_full_temphrp_ssec <- mutate(bb_full_temphrp_ssec,
  bb_nssec8_new = case_when(
    bb_jbstat != 4 & bb_jbstat != 7 & bb_jbhas == 1 | (bb_jbhas == 2 & bb_jboff == 1) ~ bb_jbnssec8_dv, # Asked if R did paid work last week, or did no paid work last week but has a job that was away from. Includes data fed forward from last year. This is the only condition for this - covers all employed.
    bb_jbstat != 4 & bb_jbstat != 7 & bb_jbhas == 2 & bb_jboff == 2 & bb_ivlyr == 1 & bb_cjsbly == 2 ~ ba_nssec8_new, # Has no job but had job in the past less than one year ago AND existing resp
    bb_jbstat != 4 & bb_jbstat != 7 & bb_jbhas == 2 & bb_jboff == 2 & bb_ivlyr != 1 & bb_cjsbly == 2 ~ bb_jlnssec8_dv, # Has no job but had job in the past less than one year ago AND new joiner
    bb_jbstat == 7 ~ 98, # Full-time students

    bb_jbstat == 4 & bb_ivlyr != 1 ~ bb_jlnssec8_dv, # Retired and new joiner
    bb_jbstat == 4 & bb_ivlyr == 1 ~ ba_nssec8_new, # Retired and existing resp

    bb_jbhas == 2 & bb_jboff == 2 & bb_cjsbly == 1 ~ 97 # Never worked or has no job but had job in the past AND been unemployed for more than a year. Previously had to divide this because of different conditions for empstend and jlend. Note never worked and LT unemployed are functionally the same category, so this is fine.
  )
)


# Read WAVE BC
file_path_bc <- file.path(base_path_bhps, "bc_indresp.dta")
bc_full_temphrp_ssec <- read_dta(file_path_bc) %>%
  dplyr::select(pidp, bc_jlnssec8_dv, bc_jbnssec8_dv, bc_jboff, bc_jbhas, bc_jbstat, bc_ivlyr, bc_cjsbly)

bb_full_temphrp_ssec2 <- dplyr::select(bb_full_temphrp_ssec, pidp, bb_nssec8_new)
bc_full_temphrp_ssec <- left_join(bc_full_temphrp_ssec, bb_full_temphrp_ssec2, by = "pidp")

bc_full_temphrp_ssec <- mutate(bc_full_temphrp_ssec,
  bc_nssec8_new = case_when(
    bc_jbstat != 4 & bc_jbstat != 7 & bc_jbhas == 1 | (bc_jbhas == 2 & bc_jboff == 1) ~ bc_jbnssec8_dv,
    bc_jbstat != 4 & bc_jbstat != 7 & bc_jbhas == 2 & bc_jboff == 2 & bc_ivlyr == 1 & bc_cjsbly == 2 ~ bb_nssec8_new,
    bc_jbstat != 4 & bc_jbstat != 7 & bc_jbhas == 2 & bc_jboff == 2 & bc_ivlyr != 1 & bc_cjsbly == 2 ~ bc_jlnssec8_dv,
    bc_jbstat == 7 ~ 98,
    bc_jbstat == 4 & bc_ivlyr != 1 ~ bc_jlnssec8_dv,
    bc_jbstat == 4 & bc_ivlyr == 1 ~ bb_nssec8_new,
    bc_jbhas == 2 & bc_jboff == 2 & bc_cjsbly == 1 ~ 97
  )
)


# Read WAVE BD
file_path_bd <- file.path(base_path_bhps, "bd_indresp.dta")
bd_full_temphrp_ssec <- read_dta(file_path_bd) %>%
  dplyr::select(pidp, bd_jlnssec8_dv, bd_jbnssec8_dv, bd_jboff, bd_jbhas, bd_jbstat, bd_ivlyr, bd_cjsbly)

bc_full_temphrp_ssec2 <- dplyr::select(bc_full_temphrp_ssec, pidp, bc_nssec8_new)
bd_full_temphrp_ssec <- left_join(bd_full_temphrp_ssec, bc_full_temphrp_ssec2, by = "pidp")

bd_full_temphrp_ssec <- mutate(bd_full_temphrp_ssec,
  bd_nssec8_new = case_when(
    bd_jbstat != 4 & bd_jbstat != 7 & bd_jbhas == 1 | (bd_jbhas == 2 & bd_jboff == 1) ~ bd_jbnssec8_dv,
    bd_jbstat != 4 & bd_jbstat != 7 & bd_jbhas == 2 & bd_jboff == 2 & bd_ivlyr == 1 & bd_cjsbly == 2 ~ bc_nssec8_new,
    bd_jbstat != 4 & bd_jbstat != 7 & bd_jbhas == 2 & bd_jboff == 2 & bd_ivlyr != 1 & bd_cjsbly == 2 ~ bd_jlnssec8_dv,
    bd_jbstat == 7 ~ 98,
    bd_jbstat == 4 & bd_ivlyr != 1 ~ bd_jlnssec8_dv,
    bd_jbstat == 4 & bd_ivlyr == 1 ~ bc_nssec8_new,
    bd_jbhas == 2 & bd_jboff == 2 & bd_cjsbly == 1 ~ 97
  )
)


# Read WAVE BE
file_path_be <- file.path(base_path_bhps, "be_indresp.dta")
be_full_temphrp_ssec <- read_dta(file_path_be) %>%
  dplyr::select(pidp, be_jlnssec8_dv, be_jbnssec8_dv, be_jboff, be_jbhas, be_jbstat, be_ivlyr, be_cjsbly)

bd_full_temphrp_ssec2 <- dplyr::select(bd_full_temphrp_ssec, pidp, bd_nssec8_new)
be_full_temphrp_ssec <- left_join(be_full_temphrp_ssec, bd_full_temphrp_ssec2, by = "pidp")

be_full_temphrp_ssec <- mutate(be_full_temphrp_ssec,
  be_nssec8_new = case_when(
    be_jbstat != 4 & be_jbstat != 7 & be_jbhas == 1 | (be_jbhas == 2 & be_jboff == 1) ~ be_jbnssec8_dv,
    be_jbstat != 4 & be_jbstat != 7 & be_jbhas == 2 & be_jboff == 2 & be_ivlyr == 1 & be_cjsbly == 2 ~ bd_nssec8_new,
    be_jbstat != 4 & be_jbstat != 7 & be_jbhas == 2 & be_jboff == 2 & be_ivlyr != 1 & be_cjsbly == 2 ~ be_jlnssec8_dv,
    be_jbstat == 7 ~ 98,
    be_jbstat == 4 & be_ivlyr != 1 ~ be_jlnssec8_dv,
    be_jbstat == 4 & be_ivlyr == 1 ~ bd_nssec8_new,
    be_jbhas == 2 & be_jboff == 2 & be_cjsbly == 1 ~ 97
  )
)


# Read WAVE BF
file_path_bf <- file.path(base_path_bhps, "bf_indresp.dta")
bf_full_temphrp_ssec <- read_dta(file_path_bf) %>%
  dplyr::select(pidp, bf_jlnssec8_dv, bf_jbnssec8_dv, bf_jboff, bf_jbhas, bf_jbstat, bf_ivlyr, bf_cjsbly)

be_full_temphrp_ssec2 <- dplyr::select(be_full_temphrp_ssec, pidp, be_nssec8_new)
bf_full_temphrp_ssec <- left_join(bf_full_temphrp_ssec, be_full_temphrp_ssec2, by = "pidp")

bf_full_temphrp_ssec <- mutate(bf_full_temphrp_ssec,
  bf_nssec8_new = case_when(
    bf_jbstat != 4 & bf_jbstat != 7 & bf_jbhas == 1 | (bf_jbhas == 2 & bf_jboff == 1) ~ bf_jbnssec8_dv,
    bf_jbstat != 4 & bf_jbstat != 7 & bf_jbhas == 2 & bf_jboff == 2 & bf_ivlyr == 1 & bf_cjsbly == 2 ~ be_nssec8_new,
    bf_jbstat != 4 & bf_jbstat != 7 & bf_jbhas == 2 & bf_jboff == 2 & bf_ivlyr != 1 & bf_cjsbly == 2 ~ bf_jlnssec8_dv,
    bf_jbstat == 7 ~ 98,
    bf_jbstat == 4 & bf_ivlyr != 1 ~ bf_jlnssec8_dv,
    bf_jbstat == 4 & bf_ivlyr == 1 ~ be_nssec8_new,
    bf_jbhas == 2 & bf_jboff == 2 & bf_cjsbly == 1 ~ 97
  )
)


# Read WAVE BG
file_path_bg <- file.path(base_path_bhps, "bg_indresp.dta")
bg_full_temphrp_ssec <- read_dta(file_path_bg) %>%
  dplyr::select(pidp, bg_jlnssec8_dv, bg_jbnssec8_dv, bg_jboff, bg_jbhas, bg_jbstat, bg_ivlyr, bg_cjsbly)

bf_full_temphrp_ssec2 <- dplyr::select(bf_full_temphrp_ssec, pidp, bf_nssec8_new)
bg_full_temphrp_ssec <- left_join(bg_full_temphrp_ssec, bf_full_temphrp_ssec2, by = "pidp")

bg_full_temphrp_ssec <- mutate(bg_full_temphrp_ssec,
  bg_nssec8_new = case_when(
    bg_jbstat != 4 & bg_jbstat != 7 & bg_jbhas == 1 | (bg_jbhas == 2 & bg_jboff == 1) ~ bg_jbnssec8_dv,
    bg_jbstat != 4 & bg_jbstat != 7 & bg_jbhas == 2 & bg_jboff == 2 & bg_ivlyr == 1 & bg_cjsbly == 2 ~ bf_nssec8_new,
    bg_jbstat != 4 & bg_jbstat != 7 & bg_jbhas == 2 & bg_jboff == 2 & bg_ivlyr != 1 & bg_cjsbly == 2 ~ bg_jlnssec8_dv,
    bg_jbstat == 7 ~ 98,
    bg_jbstat == 4 & bg_ivlyr != 1 ~ bg_jlnssec8_dv,
    bg_jbstat == 4 & bg_ivlyr == 1 ~ bf_nssec8_new,
    bg_jbhas == 2 & bg_jboff == 2 & bg_cjsbly == 1 ~ 97
  )
)


# Read WAVE BH
file_path_bh <- file.path(base_path_bhps, "bh_indresp.dta")
bh_full_temphrp_ssec <- read_dta(file_path_bh) %>%
  dplyr::select(pidp, bh_jlnssec8_dv, bh_jbnssec8_dv, bh_jboff, bh_jbhas, bh_jbstat, bh_ivlyr, bh_cjsbly)

bg_full_temphrp_ssec2 <- dplyr::select(bg_full_temphrp_ssec, pidp, bg_nssec8_new)
bh_full_temphrp_ssec <- left_join(bh_full_temphrp_ssec, bg_full_temphrp_ssec2, by = "pidp")

bh_full_temphrp_ssec <- mutate(bh_full_temphrp_ssec,
  bh_nssec8_new = case_when(
    bh_jbstat != 4 & bh_jbstat != 7 & bh_jbhas == 1 | (bh_jbhas == 2 & bh_jboff == 1) ~ bh_jbnssec8_dv,
    bh_jbstat != 4 & bh_jbstat != 7 & bh_jbhas == 2 & bh_jboff == 2 & bh_ivlyr == 1 & bh_cjsbly == 2 ~ bg_nssec8_new,
    bh_jbstat != 4 & bh_jbstat != 7 & bh_jbhas == 2 & bh_jboff == 2 & bh_ivlyr != 1 & bh_cjsbly == 2 ~ bh_jlnssec8_dv,
    bh_jbstat == 7 ~ 98,
    bh_jbstat == 4 & bh_ivlyr != 1 ~ bh_jlnssec8_dv,
    bh_jbstat == 4 & bh_ivlyr == 1 ~ bg_nssec8_new,
    bh_jbhas == 2 & bh_jboff == 2 & bh_cjsbly == 1 ~ 97
  )
)


# Read WAVE BI
file_path_bi <- file.path(base_path_bhps, "bi_indresp.dta")
bi_full_temphrp_ssec <- read_dta(file_path_bi) %>%
  dplyr::select(pidp, bi_jlnssec8_dv, bi_jbnssec8_dv, bi_jboff, bi_jbhas, bi_jbstat, bi_ivlyr, bi_cjsbly)

bh_full_temphrp_ssec2 <- dplyr::select(bh_full_temphrp_ssec, pidp, bh_nssec8_new)
bi_full_temphrp_ssec <- left_join(bi_full_temphrp_ssec, bh_full_temphrp_ssec2, by = "pidp")

bi_full_temphrp_ssec <- mutate(bi_full_temphrp_ssec,
  bi_nssec8_new = case_when(
    bi_jbstat != 4 & bi_jbstat != 7 & bi_jbhas == 1 | (bi_jbhas == 2 & bi_jboff == 1) ~ bi_jbnssec8_dv,
    bi_jbstat != 4 & bi_jbstat != 7 & bi_jbhas == 2 & bi_jboff == 2 & bi_ivlyr == 1 & bi_cjsbly == 2 ~ bh_nssec8_new,
    bi_jbstat != 4 & bi_jbstat != 7 & bi_jbhas == 2 & bi_jboff == 2 & bi_ivlyr != 1 & bi_cjsbly == 2 ~ bi_jlnssec8_dv,
    bi_jbstat == 7 ~ 98,
    bi_jbstat == 4 & bi_ivlyr != 1 ~ bi_jlnssec8_dv,
    bi_jbstat == 4 & bi_ivlyr == 1 ~ bh_nssec8_new,
    bi_jbhas == 2 & bi_jboff == 2 & bi_cjsbly == 1 ~ 97
  )
)


# Read WAVE BJ
file_path_bj <- file.path(base_path_bhps, "bj_indresp.dta")
bj_full_temphrp_ssec <- read_dta(file_path_bj) %>%
  dplyr::select(pidp, bj_jlnssec8_dv, bj_jbnssec8_dv, bj_jboff, bj_jbhas, bj_jbstat, bj_ivlyr, bj_cjsbly)

bi_full_temphrp_ssec2 <- dplyr::select(bi_full_temphrp_ssec, pidp, bi_nssec8_new)
bj_full_temphrp_ssec <- left_join(bj_full_temphrp_ssec, bi_full_temphrp_ssec2, by = "pidp")

bj_full_temphrp_ssec <- mutate(bj_full_temphrp_ssec,
  bj_nssec8_new = case_when(
    bj_jbstat != 4 & bj_jbstat != 7 & bj_jbhas == 1 | (bj_jbhas == 2 & bj_jboff == 1) ~ bj_jbnssec8_dv,
    bj_jbstat != 4 & bj_jbstat != 7 & bj_jbhas == 2 & bj_jboff == 2 & bj_ivlyr == 1 & bj_cjsbly == 2 ~ bi_nssec8_new,
    bj_jbstat != 4 & bj_jbstat != 7 & bj_jbhas == 2 & bj_jboff == 2 & bj_ivlyr != 1 & bj_cjsbly == 2 ~ bj_jlnssec8_dv,
    bj_jbstat == 7 ~ 98,
    bj_jbstat == 4 & bj_ivlyr != 1 ~ bj_jlnssec8_dv,
    bj_jbstat == 4 & bj_ivlyr == 1 ~ bi_nssec8_new,
    bj_jbhas == 2 & bj_jboff == 2 & bj_cjsbly == 1 ~ 97
  )
)


# Read WAVE BK
file_path_bk <- file.path(base_path_bhps, "bk_indresp.dta")
bk_full_temphrp_ssec <- read_dta(file_path_bk) %>%
  dplyr::select(pidp, bk_jlnssec8_dv, bk_jbnssec8_dv, bk_jboff, bk_jbhas, bk_jbstat, bk_ivlyr, bk_cjsbly)

bj_full_temphrp_ssec2 <- dplyr::select(bj_full_temphrp_ssec, pidp, bj_nssec8_new)
bk_full_temphrp_ssec <- left_join(bk_full_temphrp_ssec, bj_full_temphrp_ssec2, by = "pidp")

bk_full_temphrp_ssec <- mutate(bk_full_temphrp_ssec,
  bk_nssec8_new = case_when(
    bk_jbstat != 4 & bk_jbstat != 7 & bk_jbhas == 1 | (bk_jbhas == 2 & bk_jboff == 1) ~ bk_jbnssec8_dv,
    bk_jbstat != 4 & bk_jbstat != 7 & bk_jbhas == 2 & bk_jboff == 2 & bk_ivlyr == 1 & bk_cjsbly == 2 ~ bj_nssec8_new,
    bk_jbstat != 4 & bk_jbstat != 7 & bk_jbhas == 2 & bk_jboff == 2 & bk_ivlyr != 1 & bk_cjsbly == 2 ~ bk_jlnssec8_dv,
    bk_jbstat == 7 ~ 98,
    bk_jbstat == 4 & bk_ivlyr != 1 ~ bk_jlnssec8_dv,
    bk_jbstat == 4 & bk_ivlyr == 1 ~ bj_nssec8_new,
    bk_jbhas == 2 & bk_jboff == 2 & bk_cjsbly == 1 ~ 97
  )
)


# Read WAVE BL
file_path_bl <- file.path(base_path_bhps, "bl_indresp.dta")
bl_full_temphrp_ssec <- read_dta(file_path_bl) %>%
  dplyr::select(pidp, bl_jlnssec8_dv, bl_jbnssec8_dv, bl_jboff, bl_jbhas, bl_jbstat, bl_ivlyr, bl_cjsbly)

bk_full_temphrp_ssec2 <- dplyr::select(bk_full_temphrp_ssec, pidp, bk_nssec8_new)
bl_full_temphrp_ssec <- left_join(bl_full_temphrp_ssec, bk_full_temphrp_ssec2, by = "pidp")

bl_full_temphrp_ssec <- mutate(bl_full_temphrp_ssec,
  bl_nssec8_new = case_when(
    bl_jbstat != 4 & bl_jbstat != 7 & bl_jbhas == 1 | (bl_jbhas == 2 & bl_jboff == 1) ~ bl_jbnssec8_dv,
    bl_jbstat != 4 & bl_jbstat != 7 & bl_jbhas == 2 & bl_jboff == 2 & bl_ivlyr == 1 & bl_cjsbly == 2 ~ bk_nssec8_new,
    bl_jbstat != 4 & bl_jbstat != 7 & bl_jbhas == 2 & bl_jboff == 2 & bl_ivlyr != 1 & bl_cjsbly == 2 ~ bl_jlnssec8_dv,
    bl_jbstat == 7 ~ 98,
    bl_jbstat == 4 & bl_ivlyr != 1 ~ bl_jlnssec8_dv,
    bl_jbstat == 4 & bl_ivlyr == 1 ~ bk_nssec8_new,
    bl_jbhas == 2 & bl_jboff == 2 & bl_cjsbly == 1 ~ 97
  )
)


# Read WAVE BM
file_path_bm <- file.path(base_path_bhps, "bm_indresp.dta")
bm_full_temphrp_ssec <- read_dta(file_path_bm) %>%
  dplyr::select(pidp, bm_jlnssec8_dv, bm_jbnssec8_dv, bm_jboff, bm_jbhas, bm_jbstat, bm_ivlyr, bm_cjsbly)

bl_full_temphrp_ssec2 <- dplyr::select(bl_full_temphrp_ssec, pidp, bl_nssec8_new)
bm_full_temphrp_ssec <- left_join(bm_full_temphrp_ssec, bl_full_temphrp_ssec2, by = "pidp")

bm_full_temphrp_ssec <- mutate(bm_full_temphrp_ssec,
  bm_nssec8_new = case_when(
    bm_jbstat != 4 & bm_jbstat != 7 & bm_jbhas == 1 | (bm_jbhas == 2 & bm_jboff == 1) ~ bm_jbnssec8_dv,
    bm_jbstat != 4 & bm_jbstat != 7 & bm_jbhas == 2 & bm_jboff == 2 & bm_ivlyr == 1 & bm_cjsbly == 2 ~ bl_nssec8_new,
    bm_jbstat != 4 & bm_jbstat != 7 & bm_jbhas == 2 & bm_jboff == 2 & bm_ivlyr != 1 & bm_cjsbly == 2 ~ bm_jlnssec8_dv,
    bm_jbstat == 7 ~ 98,
    bm_jbstat == 4 & bm_ivlyr != 1 ~ bm_jlnssec8_dv,
    bm_jbstat == 4 & bm_ivlyr == 1 ~ bl_nssec8_new,
    bm_jbhas == 2 & bm_jboff == 2 & bm_cjsbly == 1 ~ 97
  )
)


# Read WAVE BN
file_path_bn <- file.path(base_path_bhps, "bn_indresp.dta")
bn_full_temphrp_ssec <- read_dta(file_path_bn) %>%
  dplyr::select(pidp, bn_jlnssec8_dv, bn_jbnssec8_dv, bn_jboff, bn_jbhas, bn_jbstat, bn_ivlyr, bn_cjsbly)

bm_full_temphrp_ssec2 <- dplyr::select(bm_full_temphrp_ssec, pidp, bm_nssec8_new)
bn_full_temphrp_ssec <- left_join(bn_full_temphrp_ssec, bm_full_temphrp_ssec2, by = "pidp")

bn_full_temphrp_ssec <- mutate(bn_full_temphrp_ssec,
  bn_nssec8_new = case_when(
    bn_jbstat != 4 & bn_jbstat != 7 & bn_jbhas == 1 | (bn_jbhas == 2 & bn_jboff == 1) ~ bn_jbnssec8_dv,
    bn_jbstat != 4 & bn_jbstat != 7 & bn_jbhas == 2 & bn_jboff == 2 & bn_ivlyr == 1 & bn_cjsbly == 2 ~ bm_nssec8_new,
    bn_jbstat != 4 & bn_jbstat != 7 & bn_jbhas == 2 & bn_jboff == 2 & bn_ivlyr != 1 & bn_cjsbly == 2 ~ bn_jlnssec8_dv,
    bn_jbstat == 7 ~ 98,
    bn_jbstat == 4 & bn_ivlyr != 1 ~ bn_jlnssec8_dv,
    bn_jbstat == 4 & bn_ivlyr == 1 ~ bm_nssec8_new,
    bn_jbhas == 2 & bn_jboff == 2 & bn_cjsbly == 1 ~ 97
  )
)


# Read WAVE BO
file_path_bo <- file.path(base_path_bhps, "bo_indresp.dta")
bo_full_temphrp_ssec <- read_dta(file_path_bo) %>%
  dplyr::select(pidp, bo_jlnssec8_dv, bo_jbnssec8_dv, bo_jboff, bo_jbhas, bo_jbstat, bo_ivlyr, bo_cjsbly)

bn_full_temphrp_ssec2 <- dplyr::select(bn_full_temphrp_ssec, pidp, bn_nssec8_new)
bo_full_temphrp_ssec <- left_join(bo_full_temphrp_ssec, bn_full_temphrp_ssec2, by = "pidp")

bo_full_temphrp_ssec <- mutate(bo_full_temphrp_ssec,
  bo_nssec8_new = case_when(
    bo_jbstat != 4 & bo_jbstat != 7 & bo_jbhas == 1 | (bo_jbhas == 2 & bo_jboff == 1) ~ bo_jbnssec8_dv,
    bo_jbstat != 4 & bo_jbstat != 7 & bo_jbhas == 2 & bo_jboff == 2 & bo_ivlyr == 1 & bo_cjsbly == 2 ~ bn_nssec8_new,
    bo_jbstat != 4 & bo_jbstat != 7 & bo_jbhas == 2 & bo_jboff == 2 & bo_ivlyr != 1 & bo_cjsbly == 2 ~ bo_jlnssec8_dv,
    bo_jbstat == 7 ~ 98,
    bo_jbstat == 4 & bo_ivlyr != 1 ~ bo_jlnssec8_dv,
    bo_jbstat == 4 & bo_ivlyr == 1 ~ bn_nssec8_new,
    bo_jbhas == 2 & bo_jboff == 2 & bo_cjsbly == 1 ~ 97
  )
)


# Read WAVE BP
file_path_bp <- file.path(base_path_bhps, "bp_indresp.dta")
bp_full_temphrp_ssec <- read_dta(file_path_bp) %>%
  dplyr::select(pidp, bp_jlnssec8_dv, bp_jbnssec8_dv, bp_jboff, bp_jbhas, bp_jbstat, bp_ivlyr, bp_cjsbly)

bo_full_temphrp_ssec2 <- dplyr::select(bo_full_temphrp_ssec, pidp, bo_nssec8_new)
bp_full_temphrp_ssec <- left_join(bp_full_temphrp_ssec, bo_full_temphrp_ssec2, by = "pidp")

bp_full_temphrp_ssec <- mutate(bp_full_temphrp_ssec,
  bp_nssec8_new = case_when(
    bp_jbstat != 4 & bp_jbstat != 7 & bp_jbhas == 1 | (bp_jbhas == 2 & bp_jboff == 1) ~ bp_jbnssec8_dv,
    bp_jbstat != 4 & bp_jbstat != 7 & bp_jbhas == 2 & bp_jboff == 2 & bp_ivlyr == 1 & bp_cjsbly == 2 ~ bo_nssec8_new,
    bp_jbstat != 4 & bp_jbstat != 7 & bp_jbhas == 2 & bp_jboff == 2 & bp_ivlyr != 1 & bp_cjsbly == 2 ~ bp_jlnssec8_dv,
    bp_jbstat == 7 ~ 98,
    bp_jbstat == 4 & bp_ivlyr != 1 ~ bp_jlnssec8_dv,
    bp_jbstat == 4 & bp_ivlyr == 1 ~ bo_nssec8_new,
    bp_jbhas == 2 & bp_jboff == 2 & bp_cjsbly == 1 ~ 97
  )
)

# Read WAVE BQ
file_path_bq <- file.path(base_path_bhps, "bq_indresp.dta")
bq_full_temphrp_ssec <- read_dta(file_path_bq) %>%
  dplyr::select(pidp, bq_jlnssec8_dv, bq_jbnssec8_dv, bq_jboff, bq_jbhas, bq_jbstat, bq_ivlyr, bq_cjsbly)

bp_full_temphrp_ssec2 <- dplyr::select(bp_full_temphrp_ssec, pidp, bp_nssec8_new)
bq_full_temphrp_ssec <- left_join(bq_full_temphrp_ssec, bp_full_temphrp_ssec2, by = "pidp")

bq_full_temphrp_ssec <- mutate(bq_full_temphrp_ssec,
  bq_nssec8_new = case_when(
    bq_jbstat != 4 & bq_jbstat != 7 & bq_jbhas == 1 | (bq_jbhas == 2 & bq_jboff == 1) ~ bq_jbnssec8_dv,
    bq_jbstat != 4 & bq_jbstat != 7 & bq_jbhas == 2 & bq_jboff == 2 & bq_ivlyr == 1 & bq_cjsbly == 2 ~ bp_nssec8_new,
    bq_jbstat != 4 & bq_jbstat != 7 & bq_jbhas == 2 & bq_jboff == 2 & bq_ivlyr != 1 & bq_cjsbly == 2 ~ bq_jlnssec8_dv,
    bq_jbstat == 7 ~ 98,
    bq_jbstat == 4 & bq_ivlyr != 1 ~ bq_jlnssec8_dv,
    bq_jbstat == 4 & bq_ivlyr == 1 ~ bp_nssec8_new,
    bq_jbhas == 2 & bq_jboff == 2 & bq_cjsbly == 1 ~ 97
  )
)


# Read WAVE BR
file_path_br <- file.path(base_path_bhps, "br_indresp.dta")
br_full_temphrp_ssec <- read_dta(file_path_br) %>%
  dplyr::select(pidp, br_jlnssec8_dv, br_jbnssec8_dv, br_jboff, br_jbhas, br_jbstat, br_ivlyr, br_cjsbly, br_ivfio)

bq_full_temphrp_ssec2 <- dplyr::select(bq_full_temphrp_ssec, pidp, bq_nssec8_new)
br_full_temphrp_ssec <- left_join(br_full_temphrp_ssec, bq_full_temphrp_ssec2, by = "pidp")

br_full_temphrp_ssec <- mutate(br_full_temphrp_ssec,
  br_nssec8_new = case_when(
    br_jbstat != 4 & br_jbstat != 7 & br_jbhas == 1 | (br_jbhas == 2 & br_jboff == 1) ~ br_jbnssec8_dv,
    br_jbstat != 4 & br_jbstat != 7 & br_jbhas == 2 & br_jboff == 2 & br_ivlyr == 1 & br_cjsbly == 2 ~ bq_nssec8_new,
    br_jbstat != 4 & br_jbstat != 7 & br_jbhas == 2 & br_jboff == 2 & br_ivlyr != 1 & br_cjsbly == 2 ~ br_jlnssec8_dv,
    br_jbstat == 7 ~ 98,
    br_jbstat == 4 & br_ivlyr != 1 ~ br_jlnssec8_dv,
    br_jbstat == 4 & br_ivlyr == 1 ~ bq_nssec8_new,
    br_jbhas == 2 & br_jboff == 2 & br_cjsbly == 1 ~ 97
  )
)


# Read WAVE A
file_path_a <- file.path(base_path_ukhls, "a_indresp.dta")
a_full_temphrp_ssec <- read_dta(file_path_a) %>%
  dplyr::select(pidp, a_jlnssec8_dv, a_jbnssec8_dv, a_jboff, a_jbhad, a_jbhas, a_jbstat, a_jlendm, a_jlendy, a_istrtdaty, a_istrtdatm)

# Create time from last employment variables, in months
# Interview date
a_full_temphrp_ssec <- mutate(a_full_temphrp_ssec,
  a_intdatemonth = (a_istrtdaty * 12 - 12) + a_istrtdatm
)
# Job date 2 (new joiners)
a_full_temphrp_ssec <- mutate(a_full_temphrp_ssec,
  a_jlendmonth = (a_jlendy * 12 - 12) + a_jlendm
)
# This is deliberately approximate - Not including day, so if resp leaves employment any time in the month 12 months before the interview, then they are counted as long-term unemployed

# Create a new NS-SeC variable (8-class), based on conditions above
a_full_temphrp_ssec <- mutate(a_full_temphrp_ssec,
  a_nssec8_new = case_when(
    a_jbstat != 4 & a_jbstat != 7 & a_jbhas == 1 | (a_jbhas == 2 & a_jboff == 1) ~ a_jbnssec8_dv, # Has a job
    a_jbstat != 4 & a_jbstat != 7 & a_jbhas == 2 & a_jboff == 2 & a_jbhad == 1 & ((a_intdatemonth - a_jlendmonth) < 12) ~ a_jlnssec8_dv, # No job but had job in the past less than a year ago

    a_jbstat == 7 ~ 98, # Full-time students
    a_jbstat == 4 ~ a_jlnssec8_dv, # Retired (Note no need to specify for first joiners for this wave)

    a_jbhas == 2 & a_jboff == 2 & a_jbhad == 1 & ((a_intdatemonth - a_jlendmonth) >= 12) ~ 97, # No job but had job in the past more than a year ago
    a_jbhas == 2 & a_jboff == 2 & a_jbhad == 2 ~ 97 # Never worked
  )
)


# Read WAVE B
file_path_b <- file.path(base_path_ukhls, "b_indresp.dta")
b_full_temphrp_ssec <- read_dta(file_path_b) %>%
  dplyr::select(pidp, b_jlnssec8_dv, b_jbnssec8_dv, b_jboff, b_jbhad, b_jbhas, b_jbsoc00chk, b_jbstat, b_jlendm, b_jlendy, b_istrtdaty, b_istrtdatm, b_notempchk)

# Read indall and merge
file_path_b_indall <- file.path(base_path_ukhls, "b_indall.dta")
b_indall_temphrp <- read_dta(file_path_b_indall) %>%
  dplyr::select(pidp, b_ff_ivlolw, b_ff_everint)
b_full_temphrp_ssec <- left_join(b_full_temphrp_ssec, b_indall_temphrp, by = "pidp")

# Do the same for BHPS cohort (where ff_ wouldn't work)
br_full_temphrp_ssec3 <- dplyr::select(br_full_temphrp_ssec, pidp, br_jbnssec8_dv, br_ivfio)
b_full_temphrp_ssec <- left_join(b_full_temphrp_ssec, br_full_temphrp_ssec3, by = "pidp")

# Feed forward variable from wave before
a_full_temphrp_ssec2 <- dplyr::select(a_full_temphrp_ssec, pidp, a_nssec8_new)
b_full_temphrp_ssec <- left_join(b_full_temphrp_ssec, a_full_temphrp_ssec2, by = "pidp")

# Feed forward variable from last BHPS wave
br_full_temphrp_ssec2 <- dplyr::select(br_full_temphrp_ssec, pidp, br_nssec8_new)
b_full_temphrp_ssec <- left_join(b_full_temphrp_ssec, br_full_temphrp_ssec2, by = "pidp")

# Create time from last employment variables, in months
# Interview date
b_full_temphrp_ssec <- mutate(b_full_temphrp_ssec,
  b_intdatemonth = (b_istrtdaty * 12 - 12) + b_istrtdatm
)
# Job date (new joiners)
b_full_temphrp_ssec <- mutate(b_full_temphrp_ssec,
  b_jlendmonth = (b_jlendy * 12 - 12) + b_jlendm
)
# Approximate - Not including day, so if resp leaves employment any time in the month 12 months before the interview, then they are counted as long-term unemployed.

# Create a new NS-SeC variable (8-class), based on conditions above
b_full_temphrp_ssec <- mutate(b_full_temphrp_ssec,
  b_nssec8_new = case_when(
    b_jbstat != 4 & b_jbstat != 7 & (b_jbhas == 1 | (b_jbhas == 2 & b_jboff == 1)) & ((b_jbsoc00chk == 2 | b_jbsoc00chk == -1) | (b_ff_ivlolw != 1 & b_ff_everint != 1)) ~ b_jbnssec8_dv, # Has a job AND (fed-forward occupation is no longer accurate OR is a new entrant never interviewed OR fed-forward occupation description is don't know, refused or missing) (not BHPS)

    b_jbstat != 4 & b_jbstat != 7 & (b_jbhas == 1 | b_jboff == 1) & (b_jbsoc00chk == 1) ~ a_nssec8_new, # Has a job AND fed-forward occupation is accurate (i.e. is existing resp) (not BHPS)
    b_jbstat != 4 & b_jbstat != 7 & (b_jbhas == 1 | (b_jbhas == 2 & b_jboff == 1)) & b_jbnssec8_dv != -9 & b_jbnssec8_dv != -8 & br_jbnssec8_dv != -9 & br_jbnssec8_dv != -8 & b_jbnssec8_dv == br_jbnssec8_dv & (br_ivfio == 1 | br_ivfio == 2 | br_ivfio == 3) ~ br_nssec8_new, # Has a job AND fed-forward occupation is accurate (i.e. is existing resp) (BHPS cohort)
    b_jbstat != 4 & b_jbstat != 7 & (b_jbhas == 1 | (b_jbhas == 2 & b_jboff == 1)) & b_jbnssec8_dv != -9 & b_jbnssec8_dv != -8 & br_jbnssec8_dv != -9 & br_jbnssec8_dv != -8 & b_jbnssec8_dv != br_jbnssec8_dv & (br_ivfio == 1 | br_ivfio == 2 | br_ivfio == 3) ~ b_jbnssec8_dv, # Has a job AND fed-forward occupation is not accurate (i.e. is existing resp) (BHPS cohort)

    b_jbstat != 4 & b_jbstat != 7 & b_jbhas == 2 & b_jboff == 2 & b_ff_ivlolw != 1 & b_ff_everint != 1 & ((b_intdatemonth - b_jlendmonth) < 12) ~ b_jlnssec8_dv, # No job but had job in the past less than one year ago AND new entrant never interviewed. This shouldn't matter whether they had job in the past (jbhad)
    b_jbstat != 4 & b_jbstat != 7 & b_jbhas == 2 & b_jboff == 2 & b_notempchk != 1 & b_ff_ivlolw == 1 ~ a_nssec8_new, # No current job AND existing respondent (not BHPS) AND not continuously unemployed since last wave
    b_jbstat != 4 & b_jbstat != 7 & b_jbhas == 2 & b_jboff == 2 & b_jbnssec8_dv != -9 & b_jbnssec8_dv != -8 & br_jbnssec8_dv != -9 & br_jbnssec8_dv != -8 & b_jbnssec8_dv == br_jbnssec8_dv & (br_ivfio == 1 | br_ivfio == 2 | br_ivfio == 3) ~ br_nssec8_new, # No current job AND existing respondent (BHPS cohort) AND not continuously unemployed since last wave (BHPS)
    b_jbstat == 7 ~ 98, # Full-time students

    b_jbstat == 4 & b_ff_ivlolw != 1 & b_ff_everint != 1 ~ b_jlnssec8_dv, # Retired and new joiner
    b_jbstat == 4 & b_ff_ivlolw == 1 ~ a_nssec8_new, # Retired and existing resp (not BHPS)
    b_jbstat == 4 & (br_ivfio == 1 | br_ivfio == 2 | br_ivfio == 3) ~ br_nssec8_new, # Retired and existing resp (BHPS cohort)

    b_jbhas == 2 & b_jboff == 2 & b_notempchk == 1 & b_ff_ivlolw == 1 ~ 97, # No current job AND existing respondent (not BHPS) AND continuously unemployed since last wave (not BHPS)
    b_jbhas == 2 & b_jboff == 2 & b_jbnssec8_dv != -9 & b_jbnssec8_dv != -8 & br_jbnssec8_dv != -9 & br_jbnssec8_dv != -8 & b_jbnssec8_dv == br_jbnssec8_dv & (br_ivfio == 1 | br_ivfio == 2 | br_ivfio == 3) ~ 97, # No current job AND existing respondent from last BHPS wave (BHPS) AND continuously unemployed since last wave (BHPS)
    b_jbhas == 2 & b_jboff == 2 & b_jbhad == 2 & b_ff_ivlolw != 1 & b_ff_everint != 1 ~ 97, # Never worked AND new joiner (jbhad only asked of new joiners). Existing respondents should be fed forward from first wave
    b_jbhas == 2 & b_jboff == 2 & b_ff_ivlolw != 1 & b_ff_everint != 1 & ((b_intdatemonth - b_jlendmonth) >= 12) ~ 97, # No job but had job in the past more than one year ago AND new entrant never interviewed. This shouldn't matter whether they had job in the past (jbhad)
  )
)


# Read WAVE C
file_path_c <- file.path(base_path_ukhls, "c_indresp.dta")
c_full_temphrp_ssec <- read_dta(file_path_c) %>%
  dplyr::select(pidp, c_jlnssec8_dv, c_jbnssec8_dv, c_jboff, c_jbhad, c_jbhas, c_jbsoc00chk, c_jbstat, c_jlendm, c_jlendy, c_istrtdaty, c_istrtdatm, c_notempchk)

file_path_c_indall <- file.path(base_path_ukhls, "c_indall.dta")
c_indall_temphrp <- read_dta(file_path_c_indall) %>%
  dplyr::select(pidp, c_ff_ivlolw, c_ff_everint)
c_full_temphrp_ssec <- left_join(c_full_temphrp_ssec, c_indall_temphrp, by = "pidp")

b_full_temphrp_ssec2 <- dplyr::select(b_full_temphrp_ssec, pidp, b_nssec8_new)
c_full_temphrp_ssec <- left_join(c_full_temphrp_ssec, b_full_temphrp_ssec2, by = "pidp")

c_full_temphrp_ssec <- mutate(c_full_temphrp_ssec,
  c_intdatemonth = (c_istrtdaty * 12 - 12) + c_istrtdatm
)

c_full_temphrp_ssec <- mutate(c_full_temphrp_ssec,
  c_jlendmonth = (c_jlendy * 12 - 12) + c_jlendm
)

c_full_temphrp_ssec <- mutate(c_full_temphrp_ssec,
  c_nssec8_new = case_when(
    c_jbstat != 4 & c_jbstat != 7 & (c_jbhas == 1 | (c_jbhas == 2 & c_jboff == 1)) & ((c_jbsoc00chk == 2 | c_jbsoc00chk == -1) | (c_ff_ivlolw != 1 & c_ff_everint != 1)) ~ c_jbnssec8_dv,
    c_jbstat != 4 & c_jbstat != 7 & (c_jbhas == 1 | c_jboff == 1) & (c_jbsoc00chk == 1) ~ b_nssec8_new,
    c_jbstat != 4 & c_jbstat != 7 & c_jbhas == 2 & c_jboff == 2 & c_ff_ivlolw != 1 & c_ff_everint != 1 & ((c_intdatemonth - c_jlendmonth) < 12) ~ c_jlnssec8_dv,
    c_jbstat != 4 & c_jbstat != 7 & c_jbhas == 2 & c_jboff == 2 & c_notempchk != 1 & c_ff_ivlolw == 1 ~ b_nssec8_new,
    c_jbstat == 7 ~ 98,
    c_jbstat == 4 & c_ff_ivlolw != 1 & c_ff_everint != 1 ~ c_jlnssec8_dv,
    c_jbstat == 4 & c_ff_ivlolw == 1 ~ b_nssec8_new,
    c_jbhas == 2 & c_jboff == 2 & c_notempchk == 1 & c_ff_ivlolw == 1 ~ 97,
    c_jbhas == 2 & c_jboff == 2 & c_jbhad == 2 & c_ff_ivlolw != 1 & c_ff_everint != 1 ~ 97,
    c_jbhas == 2 & c_jboff == 2 & c_ff_ivlolw != 1 & c_ff_everint != 1 & ((c_intdatemonth - c_jlendmonth) >= 12) ~ 97
  )
)


# Read WAVE D
file_path_d <- file.path(base_path_ukhls, "d_indresp.dta")
d_full_temphrp_ssec <- read_dta(file_path_d) %>%
  dplyr::select(pidp, d_jlnssec8_dv, d_jbnssec8_dv, d_jboff, d_jbhad, d_jbhas, d_jbsoc00chk, d_jbstat, d_jlendm, d_jlendy, d_istrtdaty, d_istrtdatm, d_notempchk)

file_path_d_indall <- file.path(base_path_ukhls, "d_indall.dta")
d_indall_temphrp <- read_dta(file_path_d_indall) %>%
  dplyr::select(pidp, d_ff_ivlolw, d_ff_everint)
d_full_temphrp_ssec <- left_join(d_full_temphrp_ssec, d_indall_temphrp, by = "pidp")

c_full_temphrp_ssec2 <- dplyr::select(c_full_temphrp_ssec, pidp, c_nssec8_new)
d_full_temphrp_ssec <- left_join(d_full_temphrp_ssec, c_full_temphrp_ssec2, by = "pidp")

d_full_temphrp_ssec <- mutate(d_full_temphrp_ssec,
  d_intdatemonth = (d_istrtdaty * 12 - 12) + d_istrtdatm
)

d_full_temphrp_ssec <- mutate(d_full_temphrp_ssec,
  d_jlendmonth = (d_jlendy * 12 - 12) + d_jlendm
)

d_full_temphrp_ssec <- mutate(d_full_temphrp_ssec,
  d_nssec8_new = case_when(
    d_jbstat != 4 & d_jbstat != 7 & (d_jbhas == 1 | (d_jbhas == 2 & d_jboff == 1)) & ((d_jbsoc00chk == 2 | d_jbsoc00chk == -1) | (d_ff_ivlolw != 1 & d_ff_everint != 1)) ~ d_jbnssec8_dv,
    d_jbstat != 4 & d_jbstat != 7 & (d_jbhas == 1 | d_jboff == 1) & (d_jbsoc00chk == 1) ~ c_nssec8_new,
    d_jbstat != 4 & d_jbstat != 7 & d_jbhas == 2 & d_jboff == 2 & d_ff_ivlolw != 1 & d_ff_everint != 1 & ((d_intdatemonth - d_jlendmonth) < 12) ~ d_jlnssec8_dv,
    d_jbstat != 4 & d_jbstat != 7 & d_jbhas == 2 & d_jboff == 2 & d_notempchk != 1 & d_ff_ivlolw == 1 ~ c_nssec8_new,
    d_jbstat == 7 ~ 98,
    d_jbstat == 4 & d_ff_ivlolw != 1 & d_ff_everint != 1 ~ d_jlnssec8_dv,
    d_jbstat == 4 & d_ff_ivlolw == 1 ~ c_nssec8_new,
    d_jbhas == 2 & d_jboff == 2 & d_notempchk == 1 & d_ff_ivlolw == 1 ~ 97,
    d_jbhas == 2 & d_jboff == 2 & d_jbhad == 2 & d_ff_ivlolw != 1 & d_ff_everint != 1 ~ 97,
    d_jbhas == 2 & d_jboff == 2 & d_ff_ivlolw != 1 & d_ff_everint != 1 & ((d_intdatemonth - d_jlendmonth) >= 12) ~ 97
  )
)


# Read WAVE E
file_path_e <- file.path(base_path_ukhls, "e_indresp.dta")
e_full_temphrp_ssec <- read_dta(file_path_e) %>%
  dplyr::select(pidp, e_jlnssec8_dv, e_jbnssec8_dv, e_jboff, e_jbhad, e_jbhas, e_jbsoc00chk, e_jbstat, e_jlendm, e_jlendy, e_istrtdaty, e_istrtdatm, e_notempchk)

file_path_e_indall <- file.path(base_path_ukhls, "e_indall.dta")
e_indall_temphrp <- read_dta(file_path_e_indall) %>%
  dplyr::select(pidp, e_ff_ivlolw, e_ff_everint)
e_full_temphrp_ssec <- left_join(e_full_temphrp_ssec, e_indall_temphrp, by = "pidp")

d_full_temphrp_ssec2 <- dplyr::select(d_full_temphrp_ssec, pidp, d_nssec8_new)
e_full_temphrp_ssec <- left_join(e_full_temphrp_ssec, d_full_temphrp_ssec2, by = "pidp")

e_full_temphrp_ssec <- mutate(e_full_temphrp_ssec,
  e_intdatemonth = (e_istrtdaty * 12 - 12) + e_istrtdatm
)

e_full_temphrp_ssec <- mutate(e_full_temphrp_ssec,
  e_jlendmonth = (e_jlendy * 12 - 12) + e_jlendm
)

e_full_temphrp_ssec <- mutate(e_full_temphrp_ssec,
  e_nssec8_new = case_when(
    e_jbstat != 4 & e_jbstat != 7 & (e_jbhas == 1 | (e_jbhas == 2 & e_jboff == 1)) & ((e_jbsoc00chk == 2 | e_jbsoc00chk == -1) | (e_ff_ivlolw != 1 & e_ff_everint != 1)) ~ e_jbnssec8_dv,
    e_jbstat != 4 & e_jbstat != 7 & (e_jbhas == 1 | e_jboff == 1) & (e_jbsoc00chk == 1) ~ d_nssec8_new,
    e_jbstat != 4 & e_jbstat != 7 & e_jbhas == 2 & e_jboff == 2 & e_ff_ivlolw != 1 & e_ff_everint != 1 & ((e_intdatemonth - e_jlendmonth) < 12) ~ e_jlnssec8_dv,
    e_jbstat != 4 & e_jbstat != 7 & e_jbhas == 2 & e_jboff == 2 & e_notempchk != 1 & e_ff_ivlolw == 1 ~ d_nssec8_new,
    e_jbstat == 7 ~ 98,
    e_jbstat == 4 & e_ff_ivlolw != 1 & e_ff_everint != 1 ~ e_jlnssec8_dv,
    e_jbstat == 4 & e_ff_ivlolw == 1 ~ d_nssec8_new,
    e_jbhas == 2 & e_jboff == 2 & e_notempchk == 1 & e_ff_ivlolw == 1 ~ 97,
    e_jbhas == 2 & e_jboff == 2 & e_jbhad == 2 & e_ff_ivlolw != 1 & e_ff_everint != 1 ~ 97,
    e_jbhas == 2 & e_jboff == 2 & e_ff_ivlolw != 1 & e_ff_everint != 1 & ((e_intdatemonth - e_jlendmonth) >= 12) ~ 97
  )
)


# Read WAVE F
file_path_f <- file.path(base_path_ukhls, "f_indresp.dta")
f_full_temphrp_ssec <- read_dta(file_path_f) %>%
  dplyr::select(pidp, f_jlnssec8_dv, f_jbnssec8_dv, f_jboff, f_jbhad, f_jbhas, f_jbsoc00chk, f_jbstat, f_jlendm, f_jlendy, f_istrtdaty, f_istrtdatm, f_notempchk)

file_path_f_indall <- file.path(base_path_ukhls, "f_indall.dta")
f_indall_temphrp <- read_dta(file_path_f_indall) %>%
  dplyr::select(pidp, f_ff_ivlolw, f_ff_everint)
f_full_temphrp_ssec <- left_join(f_full_temphrp_ssec, f_indall_temphrp, by = "pidp")

e_full_temphrp_ssec2 <- dplyr::select(e_full_temphrp_ssec, pidp, e_nssec8_new)
f_full_temphrp_ssec <- left_join(f_full_temphrp_ssec, e_full_temphrp_ssec2, by = "pidp")

f_full_temphrp_ssec <- mutate(f_full_temphrp_ssec,
  f_intdatemonth = (f_istrtdaty * 12 - 12) + f_istrtdatm
)

f_full_temphrp_ssec <- mutate(f_full_temphrp_ssec,
  f_jlendmonth = (f_jlendy * 12 - 12) + f_jlendm
)

f_full_temphrp_ssec <- mutate(f_full_temphrp_ssec,
  f_nssec8_new = case_when(
    f_jbstat != 4 & f_jbstat != 7 & (f_jbhas == 1 | (f_jbhas == 2 & f_jboff == 1)) & ((f_jbsoc00chk == 2 | f_jbsoc00chk == -1) | (f_ff_ivlolw != 1 & f_ff_everint != 1)) ~ f_jbnssec8_dv,
    f_jbstat != 4 & f_jbstat != 7 & (f_jbhas == 1 | f_jboff == 1) & (f_jbsoc00chk == 1) ~ e_nssec8_new,
    f_jbstat != 4 & f_jbstat != 7 & f_jbhas == 2 & f_jboff == 2 & f_ff_ivlolw != 1 & f_ff_everint != 1 & ((f_intdatemonth - f_jlendmonth) < 12) ~ f_jlnssec8_dv,
    f_jbstat != 4 & f_jbstat != 7 & f_jbhas == 2 & f_jboff == 2 & f_notempchk != 1 & f_ff_ivlolw == 1 ~ e_nssec8_new,
    f_jbstat == 7 ~ 98,
    f_jbstat == 4 & f_ff_ivlolw != 1 & f_ff_everint != 1 ~ f_jlnssec8_dv,
    f_jbstat == 4 & f_ff_ivlolw == 1 ~ e_nssec8_new,
    f_jbhas == 2 & f_jboff == 2 & f_notempchk == 1 & f_ff_ivlolw == 1 ~ 97,
    f_jbhas == 2 & f_jboff == 2 & f_jbhad == 2 & f_ff_ivlolw != 1 & f_ff_everint != 1 ~ 97,
    f_jbhas == 2 & f_jboff == 2 & f_ff_ivlolw != 1 & f_ff_everint != 1 & ((f_intdatemonth - f_jlendmonth) >= 12) ~ 97
  )
)


# Read WAVE G
file_path_g <- file.path(base_path_ukhls, "g_indresp.dta")
g_full_temphrp_ssec <- read_dta(file_path_g) %>%
  dplyr::select(pidp, g_jlnssec8_dv, g_jbnssec8_dv, g_jboff, g_jbhad, g_jbhas, g_jbsoc00chk, g_jbstat, g_jlendm, g_jlendy, g_istrtdaty, g_istrtdatm, g_notempchk)

file_path_g_indall <- file.path(base_path_ukhls, "g_indall.dta")
g_indall_temphrp <- read_dta(file_path_g_indall) %>%
  dplyr::select(pidp, g_ff_ivlolw, g_ff_everint)
g_full_temphrp_ssec <- left_join(g_full_temphrp_ssec, g_indall_temphrp, by = "pidp")

f_full_temphrp_ssec2 <- dplyr::select(f_full_temphrp_ssec, pidp, f_nssec8_new)
g_full_temphrp_ssec <- left_join(g_full_temphrp_ssec, f_full_temphrp_ssec2, by = "pidp")

g_full_temphrp_ssec <- mutate(g_full_temphrp_ssec,
  g_intdatemonth = (g_istrtdaty * 12 - 12) + g_istrtdatm
)

g_full_temphrp_ssec <- mutate(g_full_temphrp_ssec,
  g_jlendmonth = (g_jlendy * 12 - 12) + g_jlendm
)

g_full_temphrp_ssec <- mutate(g_full_temphrp_ssec,
  g_nssec8_new = case_when(
    g_jbstat != 4 & g_jbstat != 7 & (g_jbhas == 1 | (g_jbhas == 2 & g_jboff == 1)) & ((g_jbsoc00chk == 2 | g_jbsoc00chk == -1) | (g_ff_ivlolw != 1 & g_ff_everint != 1)) ~ g_jbnssec8_dv,
    g_jbstat != 4 & g_jbstat != 7 & (g_jbhas == 1 | g_jboff == 1) & (g_jbsoc00chk == 1) ~ f_nssec8_new,
    g_jbstat != 4 & g_jbstat != 7 & g_jbhas == 2 & g_jboff == 2 & g_ff_ivlolw != 1 & g_ff_everint != 1 & ((g_intdatemonth - g_jlendmonth) < 12) ~ g_jlnssec8_dv,
    g_jbstat != 4 & g_jbstat != 7 & g_jbhas == 2 & g_jboff == 2 & g_notempchk != 1 & g_ff_ivlolw == 1 ~ f_nssec8_new,
    g_jbstat == 7 ~ 98,
    g_jbstat == 4 & g_ff_ivlolw != 1 & g_ff_everint != 1 ~ g_jlnssec8_dv,
    g_jbstat == 4 & g_ff_ivlolw == 1 ~ f_nssec8_new,
    g_jbhas == 2 & g_jboff == 2 & g_notempchk == 1 & g_ff_ivlolw == 1 ~ 97,
    g_jbhas == 2 & g_jboff == 2 & g_jbhad == 2 & g_ff_ivlolw != 1 & g_ff_everint != 1 ~ 97,
    g_jbhas == 2 & g_jboff == 2 & g_ff_ivlolw != 1 & g_ff_everint != 1 & ((g_intdatemonth - g_jlendmonth) >= 12) ~ 97
  )
)


# Read WAVE H
file_path_h <- file.path(base_path_ukhls, "h_indresp.dta")
h_full_temphrp_ssec <- read_dta(file_path_h) %>%
  dplyr::select(pidp, h_jlnssec8_dv, h_jbnssec8_dv, h_jboff, h_jbhad, h_jbhas, h_jbsoc00chk, h_jbstat, h_jlendm, h_jlendy, h_istrtdaty, h_istrtdatm, h_notempchk)

file_path_h_indall <- file.path(base_path_ukhls, "h_indall.dta")
h_indall_temphrp <- read_dta(file_path_h_indall) %>%
  dplyr::select(pidp, h_ff_ivlolw, h_ff_everint)
h_full_temphrp_ssec <- left_join(h_full_temphrp_ssec, h_indall_temphrp, by = "pidp")

g_full_temphrp_ssec2 <- dplyr::select(g_full_temphrp_ssec, pidp, g_nssec8_new)
h_full_temphrp_ssec <- left_join(h_full_temphrp_ssec, g_full_temphrp_ssec2, by = "pidp")

h_full_temphrp_ssec <- mutate(h_full_temphrp_ssec,
  h_intdatemonth = (h_istrtdaty * 12 - 12) + h_istrtdatm
)

h_full_temphrp_ssec <- mutate(h_full_temphrp_ssec,
  h_jlendmonth = (h_jlendy * 12 - 12) + h_jlendm
)

h_full_temphrp_ssec <- mutate(h_full_temphrp_ssec,
  h_nssec8_new = case_when(
    h_jbstat != 4 & h_jbstat != 7 & (h_jbhas == 1 | (h_jbhas == 2 & h_jboff == 1)) & ((h_jbsoc00chk == 2 | h_jbsoc00chk == -1) | (h_ff_ivlolw != 1 & h_ff_everint != 1)) ~ h_jbnssec8_dv,
    h_jbstat != 4 & h_jbstat != 7 & (h_jbhas == 1 | h_jboff == 1) & (h_jbsoc00chk == 1) ~ g_nssec8_new,
    h_jbstat != 4 & h_jbstat != 7 & h_jbhas == 2 & h_jboff == 2 & h_ff_ivlolw != 1 & h_ff_everint != 1 & ((h_intdatemonth - h_jlendmonth) < 12) ~ h_jlnssec8_dv,
    h_jbstat != 4 & h_jbstat != 7 & h_jbhas == 2 & h_jboff == 2 & h_notempchk != 1 & h_ff_ivlolw == 1 ~ g_nssec8_new,
    h_jbstat == 7 ~ 98,
    h_jbstat == 4 & h_ff_ivlolw != 1 & h_ff_everint != 1 ~ h_jlnssec8_dv,
    h_jbstat == 4 & h_ff_ivlolw == 1 ~ g_nssec8_new,
    h_jbhas == 2 & h_jboff == 2 & h_notempchk == 1 & h_ff_ivlolw == 1 ~ 97,
    h_jbhas == 2 & h_jboff == 2 & h_jbhad == 2 & h_ff_ivlolw != 1 & h_ff_everint != 1 ~ 97,
    h_jbhas == 2 & h_jboff == 2 & h_ff_ivlolw != 1 & h_ff_everint != 1 & ((h_intdatemonth - h_jlendmonth) >= 12) ~ 97
  )
)


# Read WAVE I
file_path_i <- file.path(base_path_ukhls, "i_indresp.dta")
i_full_temphrp_ssec <- read_dta(file_path_i) %>%
  dplyr::select(pidp, i_jlnssec8_dv, i_jbnssec8_dv, i_jboff, i_jbhad, i_jbhas, i_jbsoc00chk, i_jbstat, i_jlendm, i_jlendy, i_istrtdaty, i_istrtdatm, i_notempchk)

file_path_i_indall <- file.path(base_path_ukhls, "i_indall.dta")
i_indall_temphrp <- read_dta(file_path_i_indall) %>%
  dplyr::select(pidp, i_ff_ivlolw, i_ff_everint)
i_full_temphrp_ssec <- left_join(i_full_temphrp_ssec, i_indall_temphrp, by = "pidp")

h_full_temphrp_ssec2 <- dplyr::select(h_full_temphrp_ssec, pidp, h_nssec8_new)
i_full_temphrp_ssec <- left_join(i_full_temphrp_ssec, h_full_temphrp_ssec2, by = "pidp")

i_full_temphrp_ssec <- mutate(i_full_temphrp_ssec,
  i_intdatemonth = (i_istrtdaty * 12 - 12) + i_istrtdatm
)

i_full_temphrp_ssec <- mutate(i_full_temphrp_ssec,
  i_jlendmonth = (i_jlendy * 12 - 12) + i_jlendm
)

i_full_temphrp_ssec <- mutate(i_full_temphrp_ssec,
  i_nssec8_new = case_when(
    i_jbstat != 4 & i_jbstat != 7 & (i_jbhas == 1 | (i_jbhas == 2 & i_jboff == 1)) & ((i_jbsoc00chk == 2 | i_jbsoc00chk == -1) | (i_ff_ivlolw != 1 & i_ff_everint != 1)) ~ i_jbnssec8_dv,
    i_jbstat != 4 & i_jbstat != 7 & (i_jbhas == 1 | i_jboff == 1) & (i_jbsoc00chk == 1) ~ h_nssec8_new,
    i_jbstat != 4 & i_jbstat != 7 & i_jbhas == 2 & i_jboff == 2 & i_ff_ivlolw != 1 & i_ff_everint != 1 & ((i_intdatemonth - i_jlendmonth) < 12) ~ i_jlnssec8_dv,
    i_jbstat != 4 & i_jbstat != 7 & i_jbhas == 2 & i_jboff == 2 & i_notempchk != 1 & i_ff_ivlolw == 1 ~ h_nssec8_new,
    i_jbstat == 7 ~ 98,
    i_jbstat == 4 & i_ff_ivlolw != 1 & i_ff_everint != 1 ~ i_jlnssec8_dv,
    i_jbstat == 4 & i_ff_ivlolw == 1 ~ h_nssec8_new,
    i_jbhas == 2 & i_jboff == 2 & i_notempchk == 1 & i_ff_ivlolw == 1 ~ 97,
    i_jbhas == 2 & i_jboff == 2 & i_jbhad == 2 & i_ff_ivlolw != 1 & i_ff_everint != 1 ~ 97,
    i_jbhas == 2 & i_jboff == 2 & i_ff_ivlolw != 1 & i_ff_everint != 1 & ((i_intdatemonth - i_jlendmonth) >= 12) ~ 97
  )
)

# Clear objects to speed up
rm(list = c("a_full_temphrp_ssec", "b_full_temphrp_ssec", "b_indall_temphrp", "b_full_temphrp_ssec2", "c_full_temphrp_ssec", "c_indall_temphrp", "c_full_temphrp_ssec2", "d_full_temphrp_ssec", "d_indall_temphrp", "d_full_temphrp_ssec2", "e_full_temphrp_ssec", "e_indall_temphrp", "e_full_temphrp_ssec2", "f_full_temphrp_ssec", "f_indall_temphrp", "f_full_temphrp_ssec2", "g_full_temphrp_ssec", "g_indall_temphrp", "g_full_temphrp_ssec2", "h_full_temphrp_ssec", "h_indall_temphrp", "h_full_temphrp_ssec2", "i_full_temphrp_ssec", "i_indall_temphrp", "i_full_temphrp_ssec2"))
rm(list = c("br_full_temphrp_ssec", "br_jobhist_temp", "bq_full_temphrp_ssec2", "bq_full_temphrp_ssec", "bq_jobhist_temp", "bp_full_temphrp_ssec2", "bp_full_temphrp_ssec", "bp_jobhist_temp", "bo_full_temphrp_ssec2", "bo_full_temphrp_ssec", "bo_jobhist_temp", "bn_full_temphrp_ssec2", "bn_full_temphrp_ssec", "bn_jobhist_temp", "bm_full_temphrp_ssec2", "bm_full_temphrp_ssec", "bm_jobhist_temp", "bl_full_temphrp_ssec2", "bl_full_temphrp_ssec", "bl_jobhist_temp", "bk_full_temphrp_ssec2", "bk_full_temphrp_ssec", "bk_jobhist_temp", "bj_full_temphrp_ssec2", "bj_full_temphrp_ssec", "bj_jobhist_temp", "bi_full_temphrp_ssec2", "bi_full_temphrp_ssec", "bi_jobhist_temp", "bh_full_temphrp_ssec2", "bh_full_temphrp_ssec", "bh_jobhist_temp", "bg_full_temphrp_ssec2", "bg_full_temphrp_ssec", "bg_jobhist_temp", "bf_full_temphrp_ssec2", "bf_full_temphrp_ssec", "bf_jobhist_temp", "be_full_temphrp_ssec2", "be_full_temphrp_ssec", "be_jobhist_temp", "bd_full_temphrp_ssec2", "bd_full_temphrp_ssec", "bd_jobhist_temp", "bc_full_temphrp_ssec2", "bc_full_temphrp_ssec", "bc_jobhist_temp", "bb_full_temphrp_ssec2", "bb_full_temphrp_ssec", "bb_jobhist_temp", "ba_full_temphrp_ssec2", "ba_full_temphrp_ssec", "ba_jobhist_temp"))
