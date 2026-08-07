


hrs16_merged <- readRDS(here::here("R_objects", "A7_005_hrs16_merged.rds"))
hrs16_22_merged <- readRDS(here::here("R_objects", "A7_005_hrs16_22_merged.rds"))

hrs16_merged <- hrs16_merged %>%
  mutate(HCAP16WGTR0 = case_when(is.na(HCAP16WGTR) ~ 0,
                                TRUE ~ HCAP16WGTR))

hrs16_nolabs <- hrs16_merged %>%
  haven::zap_labels() %>%
  mutate(mplusid = row_number()
         )

hrs16_22_nolabs <- hrs16_22_merged %>%
  haven::zap_labels() %>%
  mutate(mplusid = row_number()
  )

hrs16_nolabs <- hrs16_nolabs %>%
  mutate(vdori    = rPvdori,
         vdlfl1z  = rPvdlfl1z,
         vdlfl2   = rPvdlfl2,
         vdlfl3   = rPvdlfl3,
         vdcount  = rPvdcount,
         vdsevens = rPvdsevens,
         vdwdimm  = rPvdwdimm,
         vdwddelz = rPvdwddelz,
         vdexf7z  = rPvdexf7z   )



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


##############################
### Creating models with the item parameters fixed to model1a



model_1a_fixed_h5_path <- here::here("mplus_output", "A7", "model1a", "model1a.h5")
# mplush5::mplus.view.results(model_1a_fixed_h5_path)
model_1a_fixed_results <- mplush5::mplus.print.model.results(model_1a_fixed_h5_path)

mplus_model_1a_fixed <- model_1a_fixed_results %>%
  # filter(!grepl("Variances", Section)) %>%
  mutate(mod = case_when(Section=="" ~ str_c(Statement, " @ ", round(Estimate, 4), "; "),
                         Section=="Intercepts" ~ str_c("[", Statement, " @ ", round(Estimate, 4), "]; "),
                         Section=="Thresholds" ~ str_c("[", Statement, " @ ", round(Estimate, 4), "]; "),
                         Section=="Residual Variances" ~ str_c(Statement, " @ ", round(Estimate, 4), "; ")
  )
  ) %>%
  filter(!is.na(mod)) %>%
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


hrs16_temp <- hrs16_nolabs %>%
  left_join(model_1a_fixed_fscores, by = c("mplusid" = "MPLUSID"))

hrs16_fscores <- hrs16_temp %>%
  # mutate(F = case_when(HCAP16WGTR0==0 ~ NA_real_,
  #                      HCAP16WGTR0 >0 ~ F),
  #        F_SE = case_when(HCAP16WGTR0==0 ~ NA_real_,
  #                         HCAP16WGTR0 >0 ~ F_SE)) %>%
  select(id, F, F_SE) %>%
  rename(PF = F,
         PF_SE = F_SE) %>%
  labelled::set_variable_labels(PF = "Factor score (2016)")

hrs16_merged <- hrs16_merged %>%
  left_join(hrs16_fscores, by = c("id" = "id"))

################################################
fs::dir_create(here::here("mplus_output", "A7", "model_1a_fixed_2016_2022"))
setwd(here::here("mplus_output", "A7", "model_1a_fixed_2016_2022"))

foo <- hrs16_22_nolabs %>%
  select(mplusid, SECU_mplus, STRATUM, starts_with("inHRS"), ends_with("WGTR"),
         ends_with("vdori"),
         ends_with("vdlfl1z"),
         ends_with("vdlfl2"),
         ends_with("vdlfl3"),
         ends_with("vdcount"),
         ends_with("vdsevens"),
         ends_with("vdwdimm"),
         ends_with("vdwddelz"),
         ends_with("vdexf7z")

         )

foo_long <- foo %>%
  # pivot cognitive variables
  pivot_longer(
    cols = matches("^r(P|Q|R|S)"),
    names_to = c("wave", ".value"),
    names_pattern = "^r(P|Q|R|S)(.+)$"
  ) %>%
  # pivot inHRS columns and join
  left_join(
    foo %>%
      select(mplusid, inHRS_16, inHRS_18, inHRS_20, inHRS_22) %>%
      pivot_longer(
        cols = -mplusid,
        names_to = "wave",
        names_pattern = "inHRS_(\\d+)",
        values_to = "inHRS"
      ) %>%
      mutate(wave = case_when(
        wave == "16" ~ "P",
        wave == "18" ~ "Q",
        wave == "20" ~ "R",
        wave == "22" ~ "S"
      )),
    by = c("mplusid", "wave")
  ) %>%
  # pivot weights and join
  left_join(
    foo %>%
      select(mplusid, PWGTR, QWGTR, RWGTR, SWGTR) %>%
      pivot_longer(
        cols = -mplusid,
        names_to = "wave",
        names_pattern = "(P|Q|R|S)WGTR",
        values_to = "weight"
      ),
    by = c("mplusid", "wave")
  ) %>%
  select(mplusid, SECU_mplus, STRATUM, wave, inHRS, weight,
         vdori, vdlfl1z, vdlfl2, vdlfl3, vdcount, vdsevens,
         vdwdimm, vdwddelz, vdexf7z) %>%
  mutate(mplusid2 = row_number())



mplus_variable_2016_2022 <- gsub("HCAP16WGTR0", "weight", mplus_variable, fixed = TRUE)
mplus_variable_2016_2022 <- gsub("mplusid", "mplusid2", mplus_variable_2016_2022, fixed = TRUE)
variable_list_2016_2022 <- c(variable_list[!variable_list %in% c("HCAP16WGTR0", "mplusid")], "weight", "mplusid2")

mod_final <- MplusAutomation::mplusObject(
  TITLE = mplus_title,
  MODEL = mplus_model_1a_fixed,
  VARIABLE = mplus_variable_2016_2022,
  ANALYSIS = mplus_analysis_mlr,
  OUTPUT = mplus_output,
  SAVEDATA = "H5RESULTS = model_1a_fixed.h5; save = fscores; file = model_1a_fixed.dat;",
  usevariables = variable_list_2016_2022,
  rdata = foo_long

)
MplusAutomation::mplusModeler(mod_final, modelout = "model_1a_fixed.inp", run = 1, writeData = "always")

model_1a_fixed <- MplusAutomation::readModels("model_1a_fixed.out")
model_1a_fixed_fscores <- model_1a_fixed[["savedata"]] %>%
  tibble() %>%
  select(MPLUSID2, F, F_SE)


hrs16_22_long <- foo_long %>%
  left_join(model_1a_fixed_fscores, by = c("mplusid2" = "MPLUSID2"))  %>%
  labelled::set_variable_labels(F = "Factor score") %>%
  left_join(hrs16_22_nolabs %>%
              select(id, HHID, PN, mplusid),
            by = c("mplusid" = "mplusid")) %>%
  select(id, HHID, PN, everything())



# hrs16_22_wide_temp <- hrs16_22_long %>%
#   select(mplusid, wave, F, F_SE) %>%
#   pivot_wider(names_from = wave,
#               values_from = c(F, F_SE),
#               names_glue = "{wave}{.value}")  %>%
#   labelled::set_variable_labels(PF = "Factor score (2016)") %>%
#   labelled::set_variable_labels(QF = "Factor score (2018)") %>%
#   labelled::set_variable_labels(RF = "Factor score (2020)") %>%
#   labelled::set_variable_labels(SF = "Factor score (2022)")
#
# hrs16_22_wide_temp <- hrs16_22_nolabs %>%
#   left_join(hrs16_22_wide_temp, by = "mplusid") %>%
#   select(id, PF, QF, RF, SF, PF_SE, QF_SE, RF_SE, SF_SE)

# hrs16_22_merged <- hrs16_22_merged %>%
#   left_join(hrs16_22_wide_temp, by = "id")




saveRDS(hrs16_merged, here::here("R_objects", "A7_050_hrs16_merged.rds"))
# saveRDS(hrs16_22_merged, here::here("R_objects", "A7_050_hrs16_22_merged.rds"))
saveRDS(hrs16_22_long, here::here("R_objects", "A7_050_hrs16_22_long.rds"))







