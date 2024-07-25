
r_objects_folder <- here::here("R_objects")

dropbox_path <- fs::path_home("Library", "CloudStorage", "Dropbox")
hcap23_path <- fs::path(dropbox_path, "work", "HCAP23")
hcap23_data_path <- fs::path(hcap23_path, "POSTED", "DATA", "SOURCE")

# This dropbox folder is copied from the network folder of the same name
hrs_path <- fs::path(dropbox_path, "work", "HRS")
hrs_data_path <- fs::path(hrs_path, "POSTED", "DATA", "SOURCE")
