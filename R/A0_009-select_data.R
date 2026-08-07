


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

hrs18_cog <- h18d_r %>%
  rename(HHID = hhid,
         PN = pn,
         QSUBHH = qsubhh) %>%
  select(HHID, PN, QSUBHH,
         QD174,
         QD184,
         QD151, QD152, QD153, QD154,
         QD124, QD129,
         QD155, QD156,
         QD157, QD158,
         QD142, QD143, QD144, QD145, QD146,
         QD196, QD198)

hrs20_cog <- h20d_r %>%
  select(HHID, PN, RSUBHH,
         RD174,
         RD184,
         RD151, RD152, RD153, RD154,
         RD124, RD129,
         RD155, RD156,
         RD157, RD158,
         RD142, RD143, RD144, RD145, RD146,
         RD196, RD198, RNSSCORE)

hrs22_cog <- h22d_r %>%
  select(HHID, PN, SSUBHH,
         SD174,
         SD184,
         SD151, SD152, SD153, SD154,
         SD124, SD129,
         SD155, SD156,
         SD157, SD158,
         SD142, SD143, SD144, SD145, SD146,
         SD196, SD198, SNSSCORE)

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
# DT added PD554, PD555, PD556 - 2026-05-12
hrs16_func <- h16d_r %>%
  select(HHID, PN, PSUBHH,
         PD102, PD101,
                PD502,               PD505, PD506, PD507, PD508, PD509, PD510,
         PD511, PD512, PD513, PD514, PD515, PD516, PD517, PD518, PD519, PD520,
         PD521, PD522, PD523, PD524, PD525, PD526, PD527, PD528, PD529, PD530,
         PD531, PD532, PD533, PD534, PD535, PD536, PD537, PD538, PD539, PD540,
         PD541, PD542, PD543, PD544, PD545, PD546, PD547, PD548, PD549, PD550,
         PD551, PD552, PD553,
         PD554, PD555, PD556)

hrs18_func <- h18d_r %>%
  rename(HHID = hhid,
         PN = pn,
         QSUBHH = qsubhh) %>%
  select(HHID, PN, QSUBHH,
         QD102, QD101,
         QD502,               QD505, QD506, QD507, QD508, QD509, QD510,
         QD511, QD512, QD513, QD514, QD515, QD516, QD517, QD518, QD519, QD520,
         QD521, QD522, QD523, QD524, QD525, QD526, QD527, QD528, QD529, QD530,
         QD531, QD532, QD533, QD534, QD535, QD536, QD537, QD538, QD539, QD540,
         QD541, QD542, QD543, QD544, QD545, QD546, QD547, QD548, QD549, QD550,
         QD551, QD552, QD553,
         QD554, QD555, QD556)

hrs20_func <- h20d_r %>%
  select(HHID, PN, RSUBHH,
         RD102, RD101,
         RD502,               RD505, RD506, RD507, RD508, RD509, RD510,
         RD511, RD512, RD513, RD514, RD515, RD516, RD517, RD518, RD519, RD520,
         RD521, RD522, RD523, RD524, RD525, RD526, RD527, RD528, RD529, RD530,
         RD531, RD532, RD533, RD534, RD535, RD536, RD537, RD538, RD539, RD540,
         RD541, RD542, RD543, RD544, RD545, RD546, RD547, RD548, RD549, RD550,
         RD551, RD552, RD553,
         RD554, RD555, RD556)

hrs22_func <- h22d_r %>%
  select(HHID, PN, SSUBHH,
         SD102, SD101,
         SD502,               SD505, SD506, SD507, SD508, SD509, SD510,
         SD511, SD512, SD513, SD514, SD515, SD516, SD517, SD518, SD519, SD520,
         SD521, SD522, SD523, SD524, SD525, SD526, SD527, SD528, SD529, SD530,
         SD531, SD532, SD533, SD534, SD535, SD536, SD537, SD538, SD539, SD540,
         SD541, SD542, SD543, SD544, SD545, SD546, SD547, SD548, SD549, SD550,
         SD551, SD552, SD553,
         SD554, SD555, SD556)

# RNJ added PG013 2025-07-26
hrs16_iadl <- h16g_r %>%
  select(HHID, PN, PSUBHH,
         PG013,
         PG014, PG021, PG023, PG030, PG040, PG041, PG044, PG047, PG050, PG051, PG059)

hrs18_iadl <- h18g_r %>%
  rename(HHID = hhid,
         PN = pn,
         QSUBHH = qsubhh) %>%
  select(HHID, PN, QSUBHH,
         QG013,
         QG014, QG021, QG023, QG030, QG040, QG041, QG044, QG047, QG050, QG051, QG059)

hrs20_iadl <- h20g_r %>%
  select(HHID, PN, RSUBHH,
         RG013,
         RG014, RG021, RG023, RG030, RG040, RG041, RG044, RG047, RG050, RG051, RG059)
# No SG040 in 2022
hrs22_iadl <- h22g_r %>%
  select(HHID, PN, SSUBHH,
         SG013,
         SG014, SG021, SG023, SG030, SG041, SG044, SG047, SG050, SG051, SG059)

### Demographics & weights

h16a_r_demo <- h16a_r %>%
  select(HHID, PN, PSUBHH,
         PA019)

h18a_r_demo <- h18a_r %>%
  rename(HHID = hhid,
         PN = pn,
         QSUBHH = qsubhh) %>%
  select(HHID, PN, QSUBHH,
         QA019)

h20a_r_demo <- h20a_r %>%
  select(HHID, PN, RSUBHH,
         RA019)

h22a_r_demo <- h22a_r %>%
  select(HHID, PN, SSUBHH,
         SA019)

tracker_demo <- tracker %>%
  select(HHID, PN, PSUBHH, QSUBHH, RSUBHH, SSUBHH,
         SECU, STRATUM, HCAP16RESP,
         PWGTR, PIWTYPE, PAGE, PINSAMP, PIWWAVE, PIWYEAR, PMARST, PNURSHM, PPROXY,
         QWGTR, QIWTYPE, QAGE, QINSAMP, QIWWAVE, QIWYEAR, QMARST, QNURSHM, QPROXY,
         RWGTR, RIWTYPE, RAGE, RINSAMP, RIWWAVE, RIWYEAR, RMARST, RNURSHM, RPROXY,
         SWGTR, SIWTYPE, SAGE, SINSAMP, SIWWAVE, SIWYEAR, SMARST, SNURSHM, SPROXY,
         GENDER, HISPANIC, RACE, SCHLYRS, DEGREE, BIRTHYR)


tracker_demo <- tracker_demo %>%
  left_join(h16a_r_demo, by = c("HHID" = "HHID", "PN" = "PN", "PSUBHH" = "PSUBHH")) %>%
  left_join(h18a_r_demo, by = c("HHID" = "HHID", "PN" = "PN", "QSUBHH" = "QSUBHH")) %>%
  left_join(h20a_r_demo, by = c("HHID" = "HHID", "PN" = "PN", "RSUBHH" = "RSUBHH")) %>%
  left_join(h22a_r_demo, by = c("HHID" = "HHID", "PN" = "PN", "SSUBHH" = "SSUBHH"))




saveRDS(hrs16_cog,    here::here("R_objects", "A0_009_hrs16_cog.rds"))
saveRDS(hrs16_iadl,   here::here("R_objects", "A0_009_hrs16_iadl.rds"))
saveRDS(hrs16_func,   here::here("R_objects", "A0_009_hrs16_func.rds"))

saveRDS(hrs18_cog,    here::here("R_objects", "A0_009_hrs18_cog.rds"))
saveRDS(hrs18_iadl,   here::here("R_objects", "A0_009_hrs18_iadl.rds"))
saveRDS(hrs18_func,   here::here("R_objects", "A0_009_hrs18_func.rds"))

saveRDS(hrs20_cog,    here::here("R_objects", "A0_009_hrs20_cog.rds"))
saveRDS(hrs20_iadl,   here::here("R_objects", "A0_009_hrs20_iadl.rds"))
saveRDS(hrs20_func,   here::here("R_objects", "A0_009_hrs20_func.rds"))

saveRDS(hrs22_cog,    here::here("R_objects", "A0_009_hrs22_cog.rds"))
saveRDS(hrs22_iadl,   here::here("R_objects", "A0_009_hrs22_iadl.rds"))
saveRDS(hrs22_func,   here::here("R_objects", "A0_009_hrs22_func.rds"))
saveRDS(tracker_demo, here::here("R_objects", "A0_009_tracker_demo.rds"))

saveRDS(hrshcap,         here::here("R_objects", "A0_009_hrshcap.rds"))
saveRDS(hc16hp_r,        here::here("R_objects", "A0_009_hc16hp_r.rds"))
saveRDS(hcap_validation, here::here("R_objects", "A0_009_hcap_validation.rds"))
