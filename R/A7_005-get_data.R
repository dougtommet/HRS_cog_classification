

hrs16_merged <- readRDS(here::here("R_objects", "A0_030_hrs16_merged.rds"))
hcap16_merged <- readRDS(here::here("R_objects", "A0_030_hcap16_merged.rds"))

saveRDS(hrs16_merged, here::here("R_objects", "A7_005_hrs16_merged.rds"))
saveRDS(hcap16_merged, here::here("R_objects", "A7_005_hcap16_merged.rds"))
