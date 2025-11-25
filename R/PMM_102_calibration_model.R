

PMM_100 <- readRDS(here::here("R_objects", "PMM_100.RDS"))
mplus_mod_norms <- readRDS(here::here("R_objects", "PMM_100_mplus_mod_norms.RDS"))


inhcap <- PMM_100 %>%
  filter(inHCAP==1)

inhcap <- inhcap %>%
  mutate(id = as.numeric(id))

## Mplus model with population weights
fs::dir_create(here::here("mplus_output", "pmm_102"))
setwd(here::here("mplus_output", "pmm_102"))

mplus_mod = "
  gcp by vdori* vdlfl1z vdlfl2 vdlfl3 vdwdimmz vdwddelz vdexf7z vdsevens vdcount;
  vdwdimmz with vdwddelz;
  gcp@1;
  vdwdimmz with vdlfl1z@0 vdexf7z@0;
  vdwddelz with vdlfl1z@0 vdexf7z@0;
  vdexf7z with vdlfl1z@0;

   %cg#2%
   [gcp
        nPG014$1 nPG021$1 nPG023$1 nPG030$1 nPG040$1
        nPG041$1 nPG044$1 nPG047$1 nPG050$1 nPG059$1
        PD102$1 PD102$2];
   gcp   ;
   vdwdimmz with vdwddelz;
   %cg#3%
   [gcp
        nPG014$1 nPG021$1 nPG023$1 nPG030$1 nPG040$1
        nPG041$1 nPG044$1 nPG047$1 nPG050$1 nPG059$1
        PD102$1 PD102$2];
   gcp ;
  vdwdimmz with vdwddelz;"


mplus_mod_102 <- str_c("%OVERALL%", " \n ",
                    mplus_mod_norms, " \n ",
                    mplus_mod)


mplus_variable_102 = "categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount nPG014 nPG021
   nPG023 nPG030  nPG040 nPG041 nPG044 nPG047 nPG050 nPG059 PD102;
idvariable = id;
classes = cg (3);
knownclass = cg  (vs1hcapdxeap = 1 vs1hcapdxeap = 2 vs1hcapdxeap = 3);
weight = HCAP16WGTR; stratification = STRATUM; cluster = SECU_mplus;"

mplus_analysis_102 = "estimator = mlr;
ALGORITHM=INTEGRATION;
starts = 80;
processors = 12;
TYPE = complex mixture;"

mplus_output_102 <- "standardized;"

mod1 <- MplusAutomation::mplusObject(
  TITLE = "x1-x3: age splines
  x4: female
  x5: black
  x6: hispanic
  x7: school years",
  MODEL = mplus_mod_102,
  VARIABLE = mplus_variable_102,
  ANALYSIS = mplus_analysis_102,
  OUTPUT = mplus_output_102,
  SAVEDATA = "H5RESULTS = model_102.h5;",
  usevariables = c("id", "vs1hcapdxeap", "HCAP16WGTR", "STRATUM", "SECU_mplus",
                   "vdori", "vdlfl1z", "vdlfl2", "vdlfl3", "vdwdimmz",
                   "vdwddelz", "vdexf7z", "vdsevens", "vdcount",
                   "x1",  "x2", "x3", "x4", "x5", "x6", "x7",
                   "x1x4", "x1x5", "x1x6", "x1x7",
                   "x2x4", "x2x5", "x2x6", "x2x7",
                   "x3x4", "x3x5", "x3x6", "x3x7",
                   "x4x5", "x4x6", "x4x7",  "x5x7", "x6x7",
                   "nPG014", "nPG021", "nPG023", "nPG030",  "nPG040",
                   "nPG041", "nPG044", "nPG047", "nPG050", "nPG059",
                   "PD102"

  ),
  rdata = inhcap

)
MplusAutomation::mplusModeler(mod1, modelout = "pmm_hcap_102.inp", run = 1, writeData = "always")

# mplush5::mplus.view.results(here::here("mplus_output", "pmm_102", "model1_wt.h5"))
# mplush5::mplus.print.results.in.probability.scale(here::here("mplus_output", "pmm_102", "model1_wt.h5"))

pmm_102 <- MplusAutomation::readModels(here::here("mplus_output", "pmm_102", "pmm_hcap_102.out"))


mplus_mod_fixed <- write_lca_model(pmm_102, mplus_mod_norms)
mplus_variable_fixed = "categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount nPG014 nPG021
   nPG023 nPG030  nPG040 nPG041 nPG044 nPG047 nPG050 nPG059 PD102;
idvariable = id;
classes = c (3);
weight = HCAP16WGTR; stratification = STRATUM; cluster = SECU_mplus;"

mplus_analysis_fixed = "estimator = mlr;
ALGORITHM=INTEGRATION;
starts = 80;
processors = 12;
TYPE = complex mixture;"

mod_fixed <- MplusAutomation::mplusObject(
  TITLE = "x1-x3: age splines
  x4: female
  x5: black
  x6: hispanic
  x7: school years",
  MODEL = mplus_mod_fixed,
  VARIABLE = mplus_variable_fixed,
  ANALYSIS = mplus_analysis_fixed,
  OUTPUT = mplus_output_102,
  SAVEDATA = "H5RESULTS = model_102b.h5;
  SAVE = CPROBABILITIES; file = cprob.dat;",
  usevariables = c("id", "HCAP16WGTR", "STRATUM", "SECU_mplus",
                   "vdori", "vdlfl1z", "vdlfl2", "vdlfl3", "vdwdimmz",
                   "vdwddelz", "vdexf7z", "vdsevens", "vdcount",
                   "x1",  "x2", "x3", "x4", "x5", "x6", "x7",
                   "x1x4", "x1x5", "x1x6", "x1x7",
                   "x2x4", "x2x5", "x2x6", "x2x7",
                   "x3x4", "x3x5", "x3x6", "x3x7",
                   "x4x5", "x4x6", "x4x7",  "x5x7", "x6x7",
                   "nPG014", "nPG021", "nPG023", "nPG030",  "nPG040",
                   "nPG041", "nPG044", "nPG047", "nPG050", "nPG059",
                   "PD102"

  ),
  rdata = inhcap

)
MplusAutomation::mplusModeler(mod_fixed, modelout = "pmm_hcap_102b.inp", run = 1, writeData = "always")

