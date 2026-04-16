

tracker_demo <- readRDS(here::here("R_objects", "A0_021_tracker_demo.rds"))
langa_weir   <- readRDS(here::here("R_objects", "A0_017_langa_weir.rds"))
hudomiet     <- readRDS(here::here("R_objects", "A0_017_hudomiet.rds"))
hrs16_cog    <- readRDS(here::here("R_objects", "A0_016_hrs16_cog.rds"))
hrshcap      <- readRDS(here::here("R_objects", "A0_014_hrshcap.rds"))
hrs16_func   <- readRDS(here::here("R_objects", "A0_013_hrs16_func.rds"))
hrs16_iadl   <- readRDS(here::here("R_objects", "A0_012_hrs16_iadl.rds"))


tracker_merged <- tracker_demo %>%
  left_join(langa_weir, by = c("HHID", "PN")) %>%
  left_join(hudomiet,   by = c("HHID", "PN")) %>%
  left_join(hrs16_cog,  by = c("HHID", "PN")) %>%
  left_join(hrshcap,    by = c("HHID", "PN")) %>%
  left_join(hrs16_func, by = c("HHID", "PN")) %>%
  left_join(hrs16_iadl, by = c("HHID", "PN"))

tracker_merged <- tracker_merged %>%
  select(
    HHID, PN,
    SECU, STRATUM, PWGTR, HCAP16WGTR, samplingP,
    PINSAMP, PIWWAVE, PIWYEAR, PMARST, PNURSHM, PPROXY,
    rage, PA019, PAGE, female, black, hisp, SCHLYRS, HISPANIC, RACE,
    vdori, vdlfl1z, vdlfl2, vdlfl3, vdwdimmz, vdwddelz, vdexf7z, vdsevens, vdcount,
    rPG014, rPG021, rPG023, rPG030, rPG040, rPG041, rPG044, rPG047, rPG050, rPG059, iadl_imp,
    PG013,
    PG014,   PG021,  PG023,  PG030,  PG040,  PG041,  PG044,  PG047,  PG050,  PG059,
    PD102,
    jorm,
    normexcld, vs1hcapdx, vs1hcapdxeap, consensuspaneldx,
    cogfunction2016, # Langa-Weir
    PrDem, PrCIND, PrNorm, Cog, CogSd, Hudomiet_classification, # Hudomiet
    everything()     # All remaining variables (not yet listed)
  )




foo <- tracker_merged %>%
  mutate(not_in_hrs16 = case_when(PIWTYPE!=1 ~ PIWTYPE),
         a = 1,
         not_in_sampling_frame = case_when(nonzeroweight==0 & inHCAP==0 ~ "Weight of 0 & not in HCAP"),
         age_young_or_not_in_hcap = case_when(age65up==0 & inHCAP==0~ "Age < 65 & not in HCAP"),
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

saveRDS(tracker_merged, here::here("R_objects", "A0_030_tracker_merged.rds"))
saveRDS(hrs16_merged,   here::here("R_objects", "A0_030_hrs16_merged.rds"))
saveRDS(hcap16_merged,   here::here("R_objects", "A0_030_hcap16_merged.rds"))
saveRDS(hrs16_consort,  here::here("R_objects", "A0_030_hrs16_consort.rds"))

