# computing a sampling weight for analyses that use the consensusdx outcome

# I'm going to re-use and update the PMM_041.RDS
PMM_41 <- readRDS(here::here("R_objects", "PMM_041.RDS"))

# Multiplty the HCAP16 weight by the inverse probability of selection into the
# Consensus sample
PMM_41$HCAP16WGTR_consensus <- PMM_41$HCAP16WGTR * (PMM_41$samplingP)^(-1)
# for subpopulation using CONSENSUS sample
PMM_41 <- PMM_41 |>
  dplyr::mutate(
    inConsensus = dplyr::case_when(
      inHCAP == 0 ~ NA_real_,
      !is.na(consensuspaneldx) ~ 1,
      is.na(consensuspaneldx) ~ 0
    )
  )

# Normalize weight to sum to 100 inConsensus == 1
sumis <- PMM_41 |> dplyr::filter(inConsensus == 1) |> dplyr::pull(HCAP16WGTR_consensus) |> sum(na.rm = TRUE)
PMM_41$HCAP16WGTR_consensus <- (PMM_41$HCAP16WGTR_consensus/sumis)*100
# set weight to 0 if it is missing (samplingP must be missing, informant only cases)
# but inHCAP is 1
PMM_41 <- PMM_41 |>
  dplyr::mutate(
    HCAP16WGTR_consensus = dplyr::if_else(
      is.na(HCAP16WGTR_consensus) & inHCAP == 1,
      0,
      HCAP16WGTR_consensus
    )
  )

attr(PMM_41$inConsensus, "label") <- "Indicator for HRS/HCAP consensuspaneldx subpopulation"
attr(PMM_41$HCAP16WGTR_consensus, "label") <- "Normalized sampling weight for consensuspaneldx"


saveRDS(PMM_41, here::here("R_objects", "PMM_041.RDS"))

