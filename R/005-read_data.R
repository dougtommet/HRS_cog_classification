

hc16hp_r <- haven::read_dta(fs::path(hcap23_data_path, "HC16", "HC16sta", "hc16hp_r.dta"))

h16a_r <- haven::read_sav(fs::path(hrs_data_path, "2016", "H16A_R.sav"))
h16c_r <- haven::read_sav(fs::path(hrs_data_path, "2016", "H16C_R.sav"))
h16d_r <- haven::read_sav(fs::path(hrs_data_path, "2016", "H16D_R.sav"))

saveRDS(hc16hp_r, fs::path(r_objects_folder, "005_hc16hp_r.rds"))
saveRDS(h16d_r, fs::path(r_objects_folder, "005_h16d_r.rds"))
