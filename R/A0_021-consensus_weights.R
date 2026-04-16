
tracker_demo <- readRDS(here::here("R_objects", "A0_011_tracker_demo.rds"))


# Some people with zero weight are in HCAP!
# gtsummary::tbl_cross(tracker_demo, inHCAP, nonzeroweight)

# Nonzero weight selector

# tracker_demo <- tracker_demo %>%
#   filter(nonzeroweight == 1 | inHCAP == 1)


# gtsummary::tbl_cross(tracker_demo, HCAP16RESP, PPROXY)

# Proxy Interviews
# ==========================================================================================
#
#   PPROXY                   2016 PROXY TYPE STATUS
# Section: TR    Level: Respondent      Type: Numeric    Width: 2   Decimals: 0
#
# .................................................................................
# 520           1.  Core interview obtained, was a proxy, proxy was provided by
#                   spouse or partner
# 421           2.  Core interview obtained, was a proxy, proxy was not provided
#                   by spouse or partner
# 19971         5.  Core interview obtained, was not a proxy
# 3874          9.  Core interview not obtained
# 378          11.  Exit interview obtained, proxy provided by spouse/partner
# 933          12.  Exit interview obtained, proxy was not provided by spouse or
#                   partner, or unknown who provided proxy interview
# 333          19.  Exit interview not obtained
# 19           21.  Post-Exit interview obtained, proxy provided by
#                   spouse/partner
# 102          22.  Post-exit interview obtained, proxy was not provided by
#                   spouse or partner, or unknown who provided proxy interview
# 24          29.  Post-exit interview not obtained
# 17120       Blank.  Not in the sample this wave
#
#
# ==========================================================================================





# computing a sampling weight for analyses that use the consensusdx outcome



# Multiply the HCAP16 weight by the inverse probability of selection into the Consensus sample
tracker_demo <- tracker_demo %>%
  mutate(HCAP16WGTR_consensus = HCAP16WGTR * (samplingP)^(-1)
  )
# for subpopulation using CONSENSUS sample
tracker_demo <- tracker_demo %>%
  mutate(
    inConsensus = case_when(inHCAP == 0 ~ NA_real_,
                            !is.na(consensuspaneldx) ~ 1,
                            is.na(consensuspaneldx) ~ 0
    )
  )

# Normalize weight to sum to 100 inConsensus == 1
sumis <- tracker_demo %>%
  filter(inConsensus == 1) %>%
  pull(HCAP16WGTR_consensus) %>%
  sum(na.rm = TRUE)

tracker_demo <- tracker_demo %>%
  mutate(HCAP16WGTR_consensus = (HCAP16WGTR_consensus/sumis)*100)
# set weight to 0 if it is missing (samplingP must be missing, informant only cases)
# but inHCAP is 1
tracker_demo <- tracker_demo %>%
  mutate(HCAP16WGTR_consensus = case_when(is.na(HCAP16WGTR_consensus) & inHCAP == 1 ~ 0,
                                          TRUE ~ HCAP16WGTR_consensus
                                          )
         )

attr(tracker_demo$inConsensus, "label") <- "Indicator for HRS/HCAP consensuspaneldx subpopulation"
attr(tracker_demo$HCAP16WGTR_consensus, "label") <- "Normalized sampling weight for consensuspaneldx"


saveRDS(tracker_demo, here::here("R_objects", "A0_021_tracker_demo.rds"))

