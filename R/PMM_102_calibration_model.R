# 2026-10-22: same inputs as the core algorithm. Not used: ADL and IADL items
# (nPG014-nPG059) and immediate word recall (vdwdimmz). The GCP factor loads on
# the same 8 items as the core algorithm CFA (A7_050).


PMM_100 <- readRDS(here::here("R_objects", "PMM_100.RDS"))
mplus_mod_norms <- readRDS(here::here("R_objects", "PMM_100_mplus_mod_norms.RDS"))


inhcap <- PMM_100 %>%
  filter(inHCAP==1)

inhcap <- inhcap %>%
  mutate(id = as.numeric(id))

# Participants with at least 1 cognition-model indicator observed. Mplus
# analyzes only these, so the class priors are computed on them.
inhcap_cog <- inhcap %>%
  filter(if_any(all_of(pmm_cog_indicators), ~ !is.na(.x)))

## Mplus model with population weights
fs::dir_create(here::here("mplus_output", "pmm_102"))
setwd(here::here("mplus_output", "pmm_102"))

mplus_mod = "
  gcp by vdori* vdlfl1z vdlfl2 vdlfl3 vdwddelz vdexf7z vdsevens vdcount;
  gcp@1;
  vdwddelz with vdlfl1z@0 vdexf7z@0;
  vdexf7z with vdlfl1z@0;

   ! Identification (2026-10-22): the gcp mean is fixed at 0 in the Normal
   ! class, which also has variance 1 (gcp@1 above), so the Normal class sets
   ! the scale and the MCI and Dementia means are relative to it. With the
   ! mean free in every class and intercepts and thresholds equal across
   ! classes, the location of gcp was not identified.
   %cg#1%
   [gcp@0];
   %cg#2%
   [gcp
        PD102$1 PD102$2];
   gcp   ;
   %cg#3%
   [gcp
        PD102$1 PD102$2];
   gcp ;"


mplus_mod_102 <- str_c("%OVERALL%", " \n ",
                    mplus_mod_norms, " \n ",
                    mplus_mod)


mplus_variable_102 = "categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount PD102;
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
                   "vdori", "vdlfl1z", "vdlfl2", "vdlfl3",
                   "vdwddelz", "vdexf7z", "vdsevens", "vdcount",
                   "x1",  "x2", "x3", "x4", "x5", "x6", "x7",
                   "x1x4", "x1x5", "x1x6", "x1x7",
                   "x2x4", "x2x5", "x2x6", "x2x7",
                   "x3x4", "x3x5", "x3x6", "x3x7",
                   "x4x5", "x4x6", "x4x7",  "x5x7", "x6x7",
                   "PD102"

  ),
  rdata = inhcap

)
MplusAutomation::mplusModeler(mod1, modelout = "pmm_hcap_102.inp", run = 1, writeData = "always")

# mplush5::mplus.view.results(here::here("mplus_output", "pmm_102", "model1_wt.h5"))
# mplush5::mplus.print.results.in.probability.scale(here::here("mplus_output", "pmm_102", "model1_wt.h5"))

pmm_102 <- MplusAutomation::readModels(here::here("mplus_output", "pmm_102", "pmm_hcap_102.out"))
check_prior_sample(inhcap_cog, pmm_102, "pmm_102")


mplus_mod_fixed <- write_lca_model(pmm_102, mplus_mod_norms)
# Fix class logits at the survey-weighted HCAP class proportions, as in PMM_103,
# so that the posterior class probabilities use the weighted calibration priors.
mplus_mod_fixed <- sub("%OVERALL%",
                       str_c("%OVERALL% \n ",
                             weighted_class_logits(inhcap_cog, "vs1hcapdxeap", "HCAP16WGTR")),
                       mplus_mod_fixed, fixed = TRUE)
mplus_variable_fixed = "categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount PD102;
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
                   "vdori", "vdlfl1z", "vdlfl2", "vdlfl3",
                   "vdwddelz", "vdexf7z", "vdsevens", "vdcount",
                   "x1",  "x2", "x3", "x4", "x5", "x6", "x7",
                   "x1x4", "x1x5", "x1x6", "x1x7",
                   "x2x4", "x2x5", "x2x6", "x2x7",
                   "x3x4", "x3x5", "x3x6", "x3x7",
                   "x4x5", "x4x6", "x4x7",  "x5x7", "x6x7",
                   "PD102"

  ),
  rdata = inhcap

)
MplusAutomation::mplusModeler(mod_fixed, modelout = "pmm_hcap_102b.inp", run = 1, writeData = "always")

