


hrs16_merged <- readRDS(here::here("R_objects", "A7_005_hrs16_merged.rds"))

hrs16_merged <- hrs16_merged %>%
  mutate(HCAP16WGTR0 = case_when(is.na(HCAP16WGTR) ~ 0,
                                TRUE ~ HCAP16WGTR))

hrs16_nolabs <- hrs16_merged %>%
  haven::zap_labels() %>%
  mutate(mplusid = row_number()
         )


mplus_analysis_wlsmv = "estimator = wlsmv; COVERAGE=0; PARAMETERIZATION = THETA;
TYPE = complex;"
mplus_variable = "categorical = vdori  vdlfl2 vdlfl3 vdsevens vdcount; idvariable = mplusid;
weight = HCAP16WGTR0; stratification = STRATUM; cluster = SECU_mplus; "

mplus_output = "standardized;"
mplus_title = "vdori - Orientation to time \n
vdlfl1 - Animal naming \n
vdlfl2 - Scissors & cactus \n
vdlfl3 - President & vice-president \n
vdcount - Count backwards from 20 \n
vdsevens - Serial sevens \n
vdwdimm - Immediate word recall \n
vdwddel - Delayed word recall \n
vdexf7 - Number series \n"
mplus_model <- "f by vdori* vdlfl1z vdlfl2 vdlfl3 vdwddelz vdexf7z vdsevens vdcount;
  f @1;"
variable_list <- c("mplusid", "HCAP16WGTR0", "STRATUM", "SECU_mplus",
                   "vdori", "vdlfl1z", "vdlfl2", "vdlfl3", "vdwddelz",
                   "vdexf7z", "vdsevens", "vdcount")

# Model 1 - Removing the immediate word recall
fs::dir_create(here::here("mplus_output", "A7", "model1"))
withr::with_dir(here::here("mplus_output", "A7", "model1"), {

  mod1 <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = mplus_model,
    VARIABLE = mplus_variable,
    ANALYSIS = mplus_analysis_wlsmv,
    OUTPUT = mplus_output,
    SAVEDATA = "H5RESULTS = model1.h5;",
    usevariables = variable_list,
    rdata = hrs16_nolabs
  )
  MplusAutomation::mplusModeler(mod1, modelout = "model1.inp", run = 1, writeData = "always")

})



##############################################
# Model 1 - Removing the immediate word recall
# Fixing all item parameters
# Fitting the model with mlr/probit

fs::dir_create(here::here("mplus_output", "A7", "model1a"))

mplus_analysis_mlr = "estimator = mlr; COVERAGE=0; link=PROBIT;
TYPE = complex;"

withr::with_dir(here::here("mplus_output", "A7", "model1a"), {

  mod1a <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = mplus_model,
    VARIABLE = mplus_variable,
    ANALYSIS = mplus_analysis_mlr,
    OUTPUT = mplus_output,
    SAVEDATA = "H5RESULTS = model1a.h5;",
    usevariables = variable_list,
    rdata = hrs16_nolabs
  )
  MplusAutomation::mplusModeler(mod1a, modelout = "model1a.inp", run = 1, writeData = "always")

})




fs::dir_create(here::here("mplus_output", "A7", "model_1a_fixed"))

model_1a_fixed_h5_path <- here::here("mplus_output", "A7", "model1a", "model1a.h5")
# mplush5::mplus.view.results(model_1a_fixed_h5_path)
model_1a_fixed_results <- mplush5::mplus.print.model.results(model_1a_fixed_h5_path)

mplus_model_1a_fixed <- model_1a_fixed_results %>%
  filter(!grepl("Variances", Section)) %>%
  mutate(mod = case_when(Section=="" ~ str_c(Statement, " @ ", round(Estimate, 4), "; "),
                         Section=="Intercepts" ~ str_c("[", Statement, " @ ", round(Estimate, 4), "]; "),
                         Section=="Thresholds" ~ str_c("[", Statement, " @ ", round(Estimate, 4), "]; ")
  )
  ) %>%
  pull(mod) %>%
  str_c(collapse = " \n ")
mplus_model_1a_fixed <- str_c(mplus_model_1a_fixed, " \n [F@0]; \n F@1;")

model_1a_fixed_fscores <- withr::with_dir(here::here("mplus_output", "A7", "model_1a_fixed"), {

  mod_final <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = mplus_model_1a_fixed,
    VARIABLE = mplus_variable,
    ANALYSIS = mplus_analysis_mlr,
    OUTPUT = mplus_output,
    SAVEDATA = "H5RESULTS = model_1a_fixed.h5; save = fscores; file = model_1a_fixed.dat;",
    usevariables = variable_list,
    rdata = hrs16_nolabs
  )
  MplusAutomation::mplusModeler(mod_final, modelout = "model_1a_fixed.inp", run = 1, writeData = "always")

  model_1a_fixed <- MplusAutomation::readModels("model_1a_fixed.out")
  model_1a_fixed[["savedata"]] %>%
    tibble() %>%
    select(MPLUSID, F, F_SE)

})


hrs16_nolabs<- hrs16_nolabs %>%
  left_join(model_1a_fixed_fscores, by = c("mplusid" = "MPLUSID"))

hrs16_fscores <- hrs16_nolabs %>%
  # mutate(F = case_when(HCAP16WGTR0==0 ~ NA_real_,
  #                      HCAP16WGTR0 >0 ~ F),
  #        F_SE = case_when(HCAP16WGTR0==0 ~ NA_real_,
  #                         HCAP16WGTR0 >0 ~ F_SE)) %>%
  select(id, F, F_SE)

hrs16_merged <- hrs16_merged %>%
  left_join(hrs16_fscores, by = c("id" = "id"))

hrs16_merged <- hrs16_merged %>%
  labelled::set_variable_labels(F = "Factor score")


saveRDS(hrs16_merged, here::here("R_objects", "A7_050_hrs16_merged.rds"))





