library(MplusAutomation)

hrs16_cog <- readRDS(fs::path(r_objects_folder, "016_hrs16_cog.rds"))

hrs16_cog <- hrs16_cog %>%
  arrange(HHID, PN) %>%
  mutate(id = row_number())

hrs16_cog <- hrs16_cog %>%
  haven::zap_labels()
fs::dir_create(here::here("mplus_output"))


mplus_analysis = "estimator = wlsmv; COVERAGE=0; PARAMETERIZATION = THETA;"
mplus_variable = "categorical = vdcountz; idvariable = id;"
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


# Model 1 - All variables
fs::dir_create(here::here("mplus_output", "model1"))
setwd(here::here("mplus_output", "model1"))

mod1 <- MplusAutomation::mplusObject(
  TITLE = mplus_title,
  MODEL = "f by vdoriz* vdlfl1z vdlfl2z vdlfl3z vdwdimmz vdwddelz vdexf7z vdsevensz vdcountz;
  f @1;",
  VARIABLE = mplus_variable,
  ANALYSIS = mplus_analysis,
  OUTPUT = mplus_output,
  SAVEDATA = "H5RESULTS = model1.h5;",
  usevariables = c("vdoriz", "vdlfl1z", "vdlfl2z", "vdlfl3z", "vdwdimmz", "vdwddelz",
                   "vdexf7z", "vdsevensz", "vdcountz", "id" ),
  rdata = hrs16_cog

)
MplusAutomation::mplusModeler(mod1, modelout = "model1.inp", run = 1, writeData = "always")

# Model 2 - Removing the immediate word recall
fs::dir_create(here::here("mplus_output", "model2"))
setwd(here::here("mplus_output", "model2"))

mod2 <- MplusAutomation::mplusObject(
  TITLE = mplus_title,
  MODEL = "f by vdoriz* vdlfl1z vdlfl2z vdlfl3z vdwddelz vdexf7z vdsevensz vdcountz;
  f @1;",
  VARIABLE = mplus_variable,
  ANALYSIS = mplus_analysis,
  OUTPUT = mplus_output,
  SAVEDATA = "H5RESULTS = model2.h5;",
  usevariables = c("vdoriz", "vdlfl1z", "vdlfl2z", "vdlfl3z", "vdwddelz",
                   "vdexf7z", "vdsevensz", "vdcountz", "id" ),
  rdata = hrs16_cog

)
MplusAutomation::mplusModeler(mod2, modelout = "model2.inp", run = 1, writeData = "always")


##############################################
# Model 2 - Removing the immediate word recall
# Fixing all item parameters
# Fitting the model with mlr/probit

fs::dir_create(here::here("mplus_output", "model2a"))
setwd(here::here("mplus_output", "model2a"))
mplus_analysis = "estimator = mlr; COVERAGE=0; link=PROBIT;"

mod2a <- MplusAutomation::mplusObject(
  TITLE = mplus_title,
  MODEL = "f by vdoriz* vdlfl1z vdlfl2z vdlfl3z vdwddelz vdexf7z vdsevensz vdcountz;
  f @1;",
  VARIABLE = mplus_variable,
  ANALYSIS = mplus_analysis,
  OUTPUT = mplus_output,
  SAVEDATA = "H5RESULTS = model2a.h5;",
  usevariables = c("vdoriz", "vdlfl1z", "vdlfl2z", "vdlfl3z", "vdwddelz",
                   "vdexf7z", "vdsevensz", "vdcountz", "id" ),
  rdata = hrs16_cog

)
MplusAutomation::mplusModeler(mod2a, modelout = "model2a.inp", run = 1, writeData = "always")




fs::dir_create(here::here("mplus_output", "model_final"))
setwd(here::here("mplus_output", "model_final"))

model_final_h5_path <- here::here("mplus_output", "model2a", "model2a.h5")
# mplush5::mplus.view.results(model_final_h5_path)
model_final_results <- mplush5::mplus.print.model.results(model_final_h5_path)

mplus_model_final <- model_final_results %>%
  filter(!grepl("Variances", Section)) %>%
  mutate(mod = case_when(Section=="" ~ str_c(Statement, " @ ", round(Estimate, 4), "; "),
                         Section=="Intercepts" ~ str_c("[", Statement, " @ ", round(Estimate, 4), "]; "),
                         Section=="Thresholds" ~ str_c("[", Statement, " @ ", round(Estimate, 4), "]; ")
                         )
         ) %>%
  pull(mod) %>%
  str_c(collapse = " \n ")
mplus_model_final <- str_c(mplus_model_final, " \n [F@0]; \n F@1;")

mod_final <- MplusAutomation::mplusObject(
  TITLE = mplus_title,
  MODEL = mplus_model_final,
  VARIABLE = mplus_variable,
  ANALYSIS = mplus_analysis,
  OUTPUT = mplus_output,
  SAVEDATA = "H5RESULTS = model_final.h5; save = fscores; file = model_final.dat;",
  usevariables = c("vdoriz", "vdlfl1z", "vdlfl2z", "vdlfl3z", "vdwddelz",
                   "vdexf7z", "vdsevensz", "vdcountz", "id" ),
  rdata = hrs16_cog

)
MplusAutomation::mplusModeler(mod_final, modelout = "model_final.inp", run = 1, writeData = "always")

model_final <- MplusAutomation::readModels("model_final.out")
model_final_fscores <- model_final[["savedata"]] %>%
  select(ID, F, F_SE)

hrs16_cog <- hrs16_cog %>%
  left_join(model_final_fscores, by = c("id" = "ID"))

saveRDS(hrs16_cog, fs::path(r_objects_folder, "020_hrs16_cog.rds"))

