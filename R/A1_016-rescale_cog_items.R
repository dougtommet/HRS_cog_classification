
hrs16_cog <- readRDS(fs::path(r_objects_folder, "015_hrs16_cog.rds"))

minmax <- function(x){

  c_n <- (5 / (8*1000)) * sd(x, na.rm = T)
  c_d <- (10/ (8*1000)) * sd(x, na.rm = T)

  (x - min(x, na.rm = T) + c_n) / (max(x, na.rm = T) - min(x, na.rm = T) + c_d)

}

hrs16_cog <- hrs16_cog %>%
  mutate(
    # vdoriz = minmax(vdori),
    # vdlfl2z = minmax(vdlfl2),
    # vdlfl3z = minmax(vdlfl3),
    vdlfl1z = minmax(vdlfl1),
    vdwdimmz = minmax(vdwdimm),
    vdwddelz = minmax(vdwddel),
    vdexf7z = minmax(vdexf7),
    # vdsevensz = minmax(vdsevens),
    # vdcountz = vdcount
    )




saveRDS(hrs16_cog, fs::path(r_objects_folder, "016_hrs16_cog.rds"))

