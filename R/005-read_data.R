

hc16hp_r <- haven::read_dta(fs::path(hcap23_data_path, "HC16", "HC16sta", "hc16hp_r.dta"))

tracker <- haven::read_dta(fs::path(hrs_data_path_source, "HRS", "tracker", "trk2022v1", "trk2022tr_r.dta"))
h16a_r  <- haven::read_dta(fs::path(hrs_data_path_source, "HRS", "2016", "stata", "H16A_R.dta"))
h16c_r  <- haven::read_dta(fs::path(hrs_data_path_source, "HRS", "2016", "stata", "H16C_R.dta"))
h16d_r  <- haven::read_dta(fs::path(hrs_data_path_source, "HRS", "2016", "stata", "H16D_R.dta"))
h16g_r  <- haven::read_dta(fs::path(hrs_data_path_source, "HRS", "2016", "stata", "H16G_R.dta"))

langa_weir  <- haven::read_dta(fs::path(hrs_data_path_source, "USER", "Langa_Weir_Dementia_Classification", "LangaWeir2020v2", "cogfinalimp_9520wide.dta"))
hurd  <- haven::read_dta(fs::path(hrs_data_path_source, "USER", "Dementia_Predicted_Probabilities_Files_Hurd", "DementiaPredictedProbabilities", "pdem_withvarnames.dta"))
hudomiet <- haven::read_dta(fs::path(hrs_data_path_source, "USER", "Predicted_Cognition_and_Dementia_Measure_Hudomiet", "PredictedCognitionDementiaMeasures", "Dementia_HRS_2000-2016_Basic_Release1_2m.dta"))


normexcld <- haven::read_dta(fs::path(hrs_data_path_derived, "HCAP", "2016", "normexcld.dta"))
hrshcap   <- haven::read_dta(fs::path(hrs_data_path_derived, "HCAP", "2016", "hrshcap.dta"))
hcap_validation   <- haven::read_dta(here::here("stata", "20240228-040.dta"))


saveRDS(hc16hp_r,  fs::path(r_objects_folder, "005_hc16hp_r.rds"))
saveRDS(tracker,   fs::path(r_objects_folder, "005_tracker.rds"))
saveRDS(h16a_r,    fs::path(r_objects_folder, "005_h16a_r.rds"))
saveRDS(h16c_r,    fs::path(r_objects_folder, "005_h16c_r.rds"))
saveRDS(h16d_r,    fs::path(r_objects_folder, "005_h16d_r.rds"))
saveRDS(h16g_r,    fs::path(r_objects_folder, "005_h16g_r.rds"))
saveRDS(normexcld, fs::path(r_objects_folder, "005_normexcld.rds"))
saveRDS(hrshcap,   fs::path(r_objects_folder, "005_hrshcap.rds"))
saveRDS(langa_weir,   fs::path(r_objects_folder, "005_langa_weir.rds"))
saveRDS(hurd,   fs::path(r_objects_folder, "005_hurd.rds"))
saveRDS(hudomiet,   fs::path(r_objects_folder, "005_hudomiet.rds"))
saveRDS(hcap_validation,   fs::path(r_objects_folder, "005_hcap_validation.rds"))

