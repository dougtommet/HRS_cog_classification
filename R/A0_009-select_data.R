


tracker   <- readRDS(here::here("R_objects", "A0_005_tracker.rds"))
h16a_r    <- readRDS(here::here("R_objects", "A0_005_h16a_r.rds"))
h16d_r    <- readRDS(here::here("R_objects", "A0_005_h16d_r.rds"))
h16g_r    <- readRDS(here::here("R_objects", "A0_005_h16g_r.rds"))
normexcld <- readRDS(here::here("R_objects", "A0_005_normexcld.rds"))

hrshcap         <- readRDS(here::here("R_objects", "A0_005_hrshcap.rds"))
hc16hp_r     <- readRDS(here::here("R_objects", "A0_005_hc16hp_r.rds"))
hcap_validation <- readRDS(here::here("R_objects", "A0_005_hcap_validation.rds"))


### Cognition

hrs16_cog <- h16d_r %>%
  select(HHID, PN, PSUBHH,
         # starts_with("PD182M"), # using PD174 instead
         PD174,
         # starts_with("PD183M"), # using PD184 instead
         PD184,
         PD151, PD152, PD153, PD154,
         PD124, PD129,
         PD155, PD156,
         PD157, PD158,
         PD142, PD143, PD144, PD145, PD146,
         PD196, PD198, PNSSCORE)

hrs16_cog <- hrs16_cog %>%
  left_join(normexcld, by = c("HHID" = "hhid", "PN" = "pn"))

## HCAP classification
hrshcap <- hrshcap %>%
  select(hhid, pn, vs1hcapdx, vs1hcapdxeap,
         vs2memsc_iflag, vs2exfsc_iflag, vs2lflsc_iflag, vs2vissc_iflag, vs2vdori1_iflag, vs2memimp_iflag,
         vs2exfimp_iflag, vs2lflimp_iflag, vs2memimp_eap_iflag, vs2exfimp_eap_iflag, vs2lflimp_eap_iflag,
         vs2visimp_iflag, vs2orimp_iflag, vs3jormsc_iflag, vs3blessedsc_iflag) %>%
  rename(HHID = hhid,
         PN = pn)

# HCAP Weight
hc16hp_r <- hc16hp_r %>%
  select(hhid, pn, HCAP16WGTR) %>%
  rename(HHID = hhid,
         PN = pn)

# HRS/HCAP validation data (N=50)
hcap_validation <- hcap_validation %>%
  dplyr::select(hhid, pn, samplingP, consensuspaneldx) %>%
  rename(HHID = hhid,
         PN = pn)



### Functional impairment

# RNJ Added PD101 2025-07-21
hrs16_func <- h16d_r %>%
  select(HHID, PN, PSUBHH,
         PD102, PD101,
                PD502,               PD505, PD506, PD507, PD508, PD509, PD510,
         PD511, PD512, PD513, PD514, PD515, PD516, PD517, PD518, PD519, PD520,
         PD521, PD522, PD523, PD524, PD525, PD526, PD527, PD528, PD529, PD530,
         PD531, PD532, PD533, PD534, PD535, PD536, PD537, PD538, PD539, PD540,
         PD541, PD542, PD543, PD544, PD545, PD546, PD547, PD548, PD549, PD550,
         PD551, PD552, PD553)


# RNJ added PG013 2025-07-26
hrs16_iadl <- h16g_r %>%
  select(HHID, PN, PSUBHH,
         PG013,
         PG014, PG021, PG023, PG030, PG040, PG041, PG044, PG047, PG050, PG051, PG059)

### Demographics & weights

h16a_r_demo <- h16a_r %>%
  select(HHID, PN, PSUBHH,
         PA019)

tracker_demo <- tracker %>%
  select(HHID, PN, PSUBHH,
         SECU, STRATUM, PWGTR, HCAP16RESP, PIWTYPE,
         PAGE, PINSAMP, PIWWAVE, PIWYEAR, PMARST, PNURSHM, PPROXY,
         GENDER, HISPANIC, RACE, SCHLYRS, DEGREE, BIRTHYR)


tracker_demo <- tracker_demo %>%
  left_join(h16a_r_demo, by = c("HHID" = "HHID", "PN" = "PN", "PSUBHH" = "PSUBHH"))




saveRDS(hrs16_cog,    here::here("R_objects", "A0_009_hrs16_cog.rds"))
saveRDS(hrs16_iadl,   here::here("R_objects", "A0_009_hrs16_iadl.rds"))
saveRDS(hrs16_func,   here::here("R_objects", "A0_009_hrs16_func.rds"))
saveRDS(tracker_demo, here::here("R_objects", "A0_009_tracker_demo.rds"))

saveRDS(hrshcap,         here::here("R_objects", "A0_009_hrshcap.rds"))
saveRDS(hc16hp_r,        here::here("R_objects", "A0_009_hc16hp_r.rds"))
saveRDS(hcap_validation, here::here("R_objects", "A0_009_hcap_validation.rds"))
