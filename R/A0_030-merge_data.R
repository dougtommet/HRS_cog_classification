

tracker_demo <- readRDS(here::here("R_objects", "A0_021_tracker_demo.rds"))
langa_weir   <- readRDS(here::here("R_objects", "A0_017_langa_weir.rds"))
hudomiet     <- readRDS(here::here("R_objects", "A0_017_hudomiet.rds"))
hrshcap      <- readRDS(here::here("R_objects", "A0_014_hrshcap.rds"))
hrs16_cog    <- readRDS(here::here("R_objects", "A0_016_hrs16_cog.rds"))
hrs16_func   <- readRDS(here::here("R_objects", "A0_019_hrs16_func.rds"))
hrs16_iadl   <- readRDS(here::here("R_objects", "A0_012_hrs16_iadl.rds"))

hrs18_cog    <- readRDS(here::here("R_objects", "A0_016_hrs18_cog.rds"))
hrs18_func   <- readRDS(here::here("R_objects", "A0_019_hrs18_func.rds"))
hrs18_iadl   <- readRDS(here::here("R_objects", "A0_012_hrs18_iadl.rds"))

hrs20_cog    <- readRDS(here::here("R_objects", "A0_016_hrs20_cog.rds"))
hrs20_func   <- readRDS(here::here("R_objects", "A0_019_hrs20_func.rds"))
hrs20_iadl   <- readRDS(here::here("R_objects", "A0_012_hrs20_iadl.rds"))

hrs22_cog    <- readRDS(here::here("R_objects", "A0_016_hrs22_cog.rds"))
hrs22_func   <- readRDS(here::here("R_objects", "A0_019_hrs22_func.rds"))
hrs22_iadl   <- readRDS(here::here("R_objects", "A0_012_hrs22_iadl.rds"))


tracker_merged <- tracker_demo %>%
  left_join(langa_weir, by = c("HHID", "PN")) %>%
  left_join(hudomiet,   by = c("HHID", "PN")) %>%
  left_join(hrshcap,    by = c("HHID", "PN")) %>%
  left_join(hrs16_cog,  by = c("HHID", "PN", "PSUBHH")) %>%
  left_join(hrs16_func, by = c("HHID", "PN", "PSUBHH")) %>%
  left_join(hrs16_iadl, by = c("HHID", "PN", "PSUBHH")) %>%
  left_join(hrs18_cog,  by = c("HHID", "PN", "QSUBHH")) %>%
  left_join(hrs18_func, by = c("HHID", "PN", "QSUBHH")) %>%
  left_join(hrs18_iadl, by = c("HHID", "PN", "QSUBHH")) %>%
  left_join(hrs20_cog,  by = c("HHID", "PN", "RSUBHH")) %>%
  left_join(hrs20_func, by = c("HHID", "PN", "RSUBHH")) %>%
  left_join(hrs20_iadl, by = c("HHID", "PN", "RSUBHH")) %>%
  left_join(hrs22_cog,  by = c("HHID", "PN", "SSUBHH")) %>%
  left_join(hrs22_func, by = c("HHID", "PN", "SSUBHH")) %>%
  left_join(hrs22_iadl, by = c("HHID", "PN", "SSUBHH"))

tracker_merged <- tracker_merged %>%
  select(
    id, HHID, PN,
    SECU, STRATUM, SECU_mplus, HCAP16WGTR, samplingP, inHCAP,
    inHRS_16, inHRS_18, inHRS_20, inHRS_22, nonzeroweight16, nonzeroweight18, nonzeroweight20, nonzeroweight22,
    PWGTR, PINSAMP, PIWWAVE, PIWYEAR,
    QWGTR, QINSAMP, QIWWAVE, QIWYEAR,
    RWGTR, RINSAMP, RIWWAVE, RIWYEAR,
    SWGTR, SINSAMP, SIWWAVE, SIWYEAR,
    page, page_cat, page_cat_f,
    qage, qage_cat, qage_cat_f,
    rage, rage_cat, rage_cat_f,
    sage, sage_cat, sage_cat_f,
    female, black, hisp, SCHLYRS, HISPANIC, RACE,

    rPvdori, rPvdlfl1z, rPvdlfl2, rPvdlfl3, rPvdwdimmz, rPvdwddelz, rPvdexf7z, rPvdsevens, rPvdcount,
    rPiadl_imp, rPjorm, PD102_imp, rPD554, rPD555, rPD556,
    rQvdori, rQvdlfl1z, rQvdlfl2, rQvdlfl3, rQvdwdimmz, rQvdwddelz,            rQvdsevens, rQvdcount,
    rQiadl_imp, rQjorm, QD102_imp, rQD554, rQD555, rQD556,
    rRvdori, rRvdlfl1z, rRvdlfl2, rRvdlfl3, rRvdwdimmz, rRvdwddelz, rRvdexf7z, rRvdsevens, rRvdcount,
    rRiadl_imp, rRjorm, RD102_imp, rRD554, rRD555, rRD556,
    rSvdori, rSvdlfl1z, rSvdlfl2, rSvdlfl3, rSvdwdimmz, rSvdwddelz, rSvdexf7z, rSvdsevens, rSvdcount,
    rSiadl_imp, rSjorm, SD102_imp, rSD554, rSD555, rSD556,

    normexcld, vs1hcapdx, vs1hcapdxeap,
    inConsensus, HCAP16WGTR_consensus, consensuspaneldx,
    cogfunction2016, # Langa-Weir
    PrDem, PrCIND, PrNorm, Cog, CogSd, Hudomiet_classification, # Hudomiet

    PMARST, PNURSHM, PPROXY,
    QMARST, QNURSHM, QPROXY,
    RMARST, RNURSHM, RPROXY,
    SMARST, SNURSHM, SPROXY,
    everything()     # All remaining variables (not yet listed)
  )




foo <- tracker_merged %>%
  mutate(not_in_hrs16 = case_when(PIWTYPE!=1 ~ PIWTYPE),
         a = 1,
         not_in_sampling_frame = case_when(nonzeroweight16==0 & inHCAP==0 ~ "Weight of 0 & not in HCAP"),
         age_young_or_not_in_hcap = case_when(age65up16==0 & inHCAP==0~ "Age < 65 & not in HCAP"),
         not_in_hcap = case_when(inHCAP==0 ~ "Not in HCAP")
         )

hrs16_consort <- foo %>%
  consort::consort_plot(order = list(c(id = "Tracker file"),
                            c(not_in_hrs16 = "PIWTYPE"),
                            c(a = "HRS 2016"),
                            c(not_in_sampling_frame = "Not in sampling frame"),
                            c(a = "Non-zero sampling weight or in HCAP"),
                            c(age_young_or_not_in_hcap = "Age"),
                            c(a = "HRS 2016 wave, age 65+"),
                            c(not_in_hcap = "Not in HCAP (2016)"),
                            c(a = "HCAP (2016)")
                            ),
               side_box = c("not_in_hrs16", "not_in_sampling_frame", "age_young_or_not_in_hcap", "not_in_hcap")
               )

hrs16_merged <- foo %>%
  filter(is.na(not_in_hrs16)) %>%
  filter(is.na(not_in_sampling_frame)) %>%
  filter(is.na(age_young_or_not_in_hcap))

hcap16_merged <- hrs16_merged %>%
  filter(is.na(not_in_hcap)) %>%
  select(-not_in_hrs16, -a, not_in_sampling_frame, -age_young_or_not_in_hcap, -not_in_hcap)

hrs16_merged <- hrs16_merged %>%
  select(-not_in_hrs16, -a, not_in_sampling_frame, -age_young_or_not_in_hcap, -not_in_hcap)


hrs16_22_merged <- tracker_merged %>%
  filter(inHRS_16==1 | inHRS_18==1 | inHRS_20==1 | inHRS_22==1)

saveRDS(tracker_merged,  here::here("R_objects", "A0_030_tracker_merged.rds"))
saveRDS(hrs16_merged,    here::here("R_objects", "A0_030_hrs16_merged.rds"))
saveRDS(hcap16_merged,   here::here("R_objects", "A0_030_hcap16_merged.rds"))
saveRDS(hrs16_consort,   here::here("R_objects", "A0_030_hrs16_consort.rds"))
saveRDS(hrs16_22_merged,    here::here("R_objects", "A0_030_hrs16_22_merged.rds"))

