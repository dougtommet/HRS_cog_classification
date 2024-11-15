
r_objects_folder <- here::here("R_objects")

dropbox_path <- fs::path_home("Library", "CloudStorage", "Dropbox")
hcap23_data_path <- fs::path(dropbox_path, "work", "HCAP23", "POSTED", "DATA", "SOURCE")

# This dropbox folder is copied from the network folder of the same name
# It also has the tracker & 2016 core data downloaded from the HRS site
# It also has the normexcld data sent by RNJ
hrs_data_path <- fs::path(dropbox_path, "work", "HRS_data", "POSTED", "DATA", "SOURCE")



