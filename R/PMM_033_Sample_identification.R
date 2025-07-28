source(here::here("R","PMM_027_custom_functions.R"))
PMM_33 <- readRDS(here::here("R_objects", "PMM_031.RDS"))
# nrow(PMM_33)
# Nonzero weight indicator
PMM_33$nonzeroweight <- ifelse(is.na(PMM_33$PWGTR), 0, ifelse(PMM_33$PWGTR > 0, 1, 0))
# in HCAP sample
PMM_33$inHCAP <- ifelse(!is.na(PMM_33$vs1hcapdx), 1, 0)
# Some people with zero weight are in HCAP! Major Bum!
# Nonzero weight selector
# PMM_33 |> tab2(inHCAP,nonzeroweight)
PMM_33 <- dplyr::filter(PMM_33, nonzeroweight == 1 | inHCAP == 1)
# nrow(PMM_33)
PMM_33$age65up <- dplyr::case_when(
  is.na(PMM_33$rage) ~ NA_real_,
  PMM_33$rage >= 65  ~ 1,
  PMM_33$rage < 65   ~ 0
)
# PMM_33 |> tab1(age65up)
# Proxy Interviews
# PMM_33 |> dplyr::filter(age65up ==1) |> tab1(PPROXY)
# PPROXY                   2016 PROXY TYPE STATUS
# Section: TR    Level: Respondent      Type: Numeric    Width: 2   Decimals: 0
#
# .................................................................................
#  286         1.  Core interview obtained, was a proxy, proxy was provided by
#                   spouse or partner
#  285         2.  Core interview obtained, was a proxy, proxy was not provided
#                   by spouse or partner
# 9192         5.  Core interview obtained, was not a proxy
# Number of people in HCAP sample who have a 2016 Core interview by Proxy
# PMM_33 |> dplyr::filter(age65up ==1, PPROXY %in% c(1,2) , inHCAP==1 ) |> nrow()
# SUBPOPULATION INDICATORS
# inHCAP == 1
# PMM_33 |> dplyr::filter(inHCAP==1 ) |> nrow()
# age65up == 1
# PMM_33 |> dplyr::filter(age65up==1 ) |> nrow()
# robust norms group normexcld == 0
# PMM_33 |> dplyr::filter(normexcld == 0 ) |> nrow()
## Consensus panel not in RDSs
PMM_33$inHCAP <- ifelse(!is.na(PMM_33$vs1hcapdx), 1, 0)
# PMM_33 |> tab1(inHCAP)
# PMM_33 |> dplyr::filter(age65up ==1, PPROXY %in% c(1,2) , inHCAP==1 ) |> tab1(vs1hcapdxeap)
# Save the merged data
saveRDS(PMM_33, here::here("R_objects", "PMM_033.RDS"))
# have a nice day


