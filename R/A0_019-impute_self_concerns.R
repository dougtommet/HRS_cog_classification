


tracker_demo <- readRDS(here::here("R_objects", "A0_021_tracker_demo.rds"))

hrs16_cog    <- readRDS(here::here("R_objects", "A0_016_hrs16_cog.rds"))
hrs16_func   <- readRDS(here::here("R_objects", "A0_013_hrs16_func.rds"))
hrs16_iadl   <- readRDS(here::here("R_objects", "A0_012_hrs16_iadl.rds"))

hrs18_cog    <- readRDS(here::here("R_objects", "A0_016_hrs18_cog.rds"))
hrs18_func   <- readRDS(here::here("R_objects", "A0_013_hrs18_func.rds"))
hrs18_iadl   <- readRDS(here::here("R_objects", "A0_012_hrs18_iadl.rds"))

hrs20_cog    <- readRDS(here::here("R_objects", "A0_016_hrs20_cog.rds"))
hrs20_func   <- readRDS(here::here("R_objects", "A0_013_hrs20_func.rds"))
hrs20_iadl   <- readRDS(here::here("R_objects", "A0_012_hrs20_iadl.rds"))

hrs22_cog    <- readRDS(here::here("R_objects", "A0_016_hrs22_cog.rds"))
hrs22_func   <- readRDS(here::here("R_objects", "A0_013_hrs22_func.rds"))
hrs22_iadl   <- readRDS(here::here("R_objects", "A0_012_hrs22_iadl.rds"))

hrs16_cog_small <- hrs16_cog %>%
  select(HHID, PN, contains("SUBHH"), contains("vdcount"), contains("vdlfl1"),
         contains("vdwdimm"), contains("vdwddel"), contains("vdexf7"), contains("vdori"),
         contains("vdlfl2"), contains("vdlfl3"), contains("vdsevens"))

hrs16_func_small <- hrs16_func %>%
  select(HHID, PN, contains("SUBHH"), contains("jorm1"), contains("jorm2"),
         contains("jorm3"), contains("jorm4"), contains("jorm5"), contains("jorm6"),
         contains("jorm7"), contains("jorm8"), contains("jorm9"), contains("jorm10"),
         contains("jorm11"), contains("jorm12"), contains("jorm13"), contains("jorm14"),
         contains("jorm15"), contains("jorm16"), contains("D102"),
         contains("D554"), contains("D555"), contains("D556"))

hrs16_iadl_small <- hrs16_iadl %>%
  select(HHID, PN, contains("SUBHH"), contains("vdcount"), contains("G014"),
         contains("G021"), contains("G023"), contains("G030"), contains("G040"),
         contains("G041"), contains("G044"), contains("G047"), contains("G050"),
         contains("G059")) %>%
  select(HHID, PN, contains("SUBHH"), starts_with("r"))

hrs18_cog_small <- hrs18_cog %>%
  select(HHID, PN, contains("SUBHH"), contains("vdcount"), contains("vdlfl1"),
         contains("vdwdimm"), contains("vdwddel"), contains("vdexf7"), contains("vdori"),
         contains("vdlfl2"), contains("vdlfl3"), contains("vdsevens"))

hrs18_func_small <- hrs18_func %>%
  select(HHID, PN, contains("SUBHH"), contains("jorm1"), contains("jorm2"),
         contains("jorm3"), contains("jorm4"), contains("jorm5"), contains("jorm6"),
         contains("jorm7"), contains("jorm8"), contains("jorm9"), contains("jorm10"),
         contains("jorm11"), contains("jorm12"), contains("jorm13"), contains("jorm14"),
         contains("jorm15"), contains("jorm16"), contains("D102"),
         contains("D554"), contains("D555"), contains("D556"))

hrs18_iadl_small <- hrs18_iadl %>%
  select(HHID, PN, contains("SUBHH"), contains("vdcount"), contains("G014"),
         contains("G021"), contains("G023"), contains("G030"), contains("G040"),
         contains("G041"), contains("G044"), contains("G047"), contains("G050"),
         contains("G059")) %>%
  select(HHID, PN, contains("SUBHH"), starts_with("r"))

hrs20_cog_small <- hrs20_cog %>%
  select(HHID, PN, contains("SUBHH"), contains("vdcount"), contains("vdlfl1"),
         contains("vdwdimm"), contains("vdwddel"), contains("vdexf7"), contains("vdori"),
         contains("vdlfl2"), contains("vdlfl3"), contains("vdsevens"))

hrs20_func_small <- hrs20_func %>%
  select(HHID, PN, contains("SUBHH"), contains("jorm1"), contains("jorm2"),
         contains("jorm3"), contains("jorm4"), contains("jorm5"), contains("jorm6"),
         contains("jorm7"), contains("jorm8"), contains("jorm9"), contains("jorm10"),
         contains("jorm11"), contains("jorm12"), contains("jorm13"), contains("jorm14"),
         contains("jorm15"), contains("jorm16"), contains("D102"),
         contains("D554"), contains("D555"), contains("D556"))

hrs20_iadl_small <- hrs20_iadl %>%
  select(HHID, PN, contains("SUBHH"), contains("vdcount"), contains("G014"),
         contains("G021"), contains("G023"), contains("G030"), contains("G040"),
         contains("G041"), contains("G044"), contains("G047"), contains("G050"),
         contains("G059")) %>%
  select(HHID, PN, contains("SUBHH"), starts_with("r"))

hrs22_cog_small <- hrs22_cog %>%
  select(HHID, PN, contains("SUBHH"), contains("vdcount"), contains("vdlfl1"),
         contains("vdwdimm"), contains("vdwddel"), contains("vdexf7"), contains("vdori"),
         contains("vdlfl2"), contains("vdlfl3"), contains("vdsevens"))

hrs22_func_small <- hrs22_func %>%
  select(HHID, PN, contains("SUBHH"), contains("jorm1"), contains("jorm2"),
         contains("jorm3"), contains("jorm4"), contains("jorm5"), contains("jorm6"),
         contains("jorm7"), contains("jorm8"), contains("jorm9"), contains("jorm10"),
         contains("jorm11"), contains("jorm12"), contains("jorm13"), contains("jorm14"),
         contains("jorm15"), contains("jorm16"), contains("D102"),
         contains("D554"), contains("D555"), contains("D556"))

hrs22_iadl_small <- hrs22_iadl %>%
  select(HHID, PN, contains("SUBHH"), contains("vdcount"), contains("G014"),
         contains("G021"), contains("G023"), contains("G030"), contains("G040"),
         contains("G041"), contains("G044"), contains("G047"), contains("G050"),
         contains("G059")) %>%
  select(HHID, PN, contains("SUBHH"), starts_with("r"))

tracker_16 <- tracker_demo %>%
  filter(inHRS_16==1) %>%
  select(HHID, PN, PSUBHH,
         xP1, xP2, xP3, xP4, xP5, xP6, xP7,
         xP1xP4, xP1xP5, xP1xP6, xP1xP7, xP2xP4,
         xP2xP5, xP2xP6, xP2xP7, xP3xP4, xP3xP5, xP3xP6,
         xP3xP7, xP4xP5, xP4xP6, xP4xP7, xP5xP7, xP6xP7) %>%
  left_join(hrs16_cog_small,  by = c("HHID", "PN", "PSUBHH")) %>%
  left_join(hrs16_func_small, by = c("HHID", "PN", "PSUBHH")) %>%
  left_join(hrs16_iadl_small, by = c("HHID", "PN", "PSUBHH")) %>%
  # select(-HHID, -PN, -PSUBHH) %>%
  haven::zap_labels()

tracker_18 <- tracker_demo %>%
  filter(inHRS_18==1) %>%
  select(HHID, PN, QSUBHH,
         xQ1, xQ2, xQ3, xQ4, xQ5, xQ6, xQ7,
         xQ1xQ4, xQ1xQ5, xQ1xQ6, xQ1xQ7, xQ2xQ4,
         xQ2xQ5, xQ2xQ6, xQ2xQ7, xQ3xQ4, xQ3xQ5, xQ3xQ6,
         xQ3xQ7, xQ4xQ5, xQ4xQ6, xQ4xQ7, xQ5xQ7, xQ6xQ7) %>%
  left_join(hrs18_cog_small,  by = c("HHID", "PN", "QSUBHH")) %>%
  left_join(hrs18_func_small, by = c("HHID", "PN", "QSUBHH")) %>%
  left_join(hrs18_iadl_small, by = c("HHID", "PN", "QSUBHH")) %>%
  # select(-HHID, -PN, -QSUBHH) %>%
  haven::zap_labels()

tracker_20 <- tracker_demo %>%
  filter(inHRS_20==1) %>%
  select(HHID, PN, RSUBHH,
         xR1, xR2, xR3, xR4, xR5, xR6, xR7,
         xR1xR4, xR1xR5, xR1xR6, xR1xR7, xR2xR4,
         xR2xR5, xR2xR6, xR2xR7, xR3xR4, xR3xR5, xR3xR6,
         xR3xR7, xR4xR5, xR4xR6, xR4xR7, xR5xR7, xR6xR7) %>%
  left_join(hrs20_cog_small,  by = c("HHID", "PN", "RSUBHH")) %>%
  left_join(hrs20_func_small, by = c("HHID", "PN", "RSUBHH")) %>%
  left_join(hrs20_iadl_small, by = c("HHID", "PN", "RSUBHH")) %>%
  # select(-HHID, -PN, -RSUBHH) %>%
  haven::zap_labels()

tracker_22 <- tracker_demo %>%
  filter(inHRS_22==1) %>%
  select(HHID, PN, SSUBHH,
         xS1, xS2, xS3, xS4, xS5, xS6, xS7,
         xS1xS4, xS1xS5, xS1xS6, xS1xS7, xS2xS4,
         xS2xS5, xS2xS6, xS2xS7, xS3xS4, xS3xS5, xS3xS6,
         xS3xS7, xS4xS5, xS4xS6, xS4xS7, xS5xS7, xS6xS7) %>%
  left_join(hrs22_cog_small,  by = c("HHID", "PN", "SSUBHH")) %>%
  left_join(hrs22_func_small, by = c("HHID", "PN", "SSUBHH")) %>%
  left_join(hrs22_iadl_small, by = c("HHID", "PN", "SSUBHH")) %>%
  # select(-HHID, -PN, -SSUBHH) %>%
  haven::zap_labels()

# impute_vars <- c(
# "xP1", "xP2", "xP3", "xP4", "xP5", "xP6", "xP7",
# "xP1xP4", "xP1xP5", "xP1xP6", "xP1xP7", "xP2xP4",
# "xP2xP5", "xP2xP6", "xP2xP7", "xP3xP4", "xP3xP5", "xP3xP6",
# "xP3xP7", "xP4xP5", "xP4xP6", "xP4xP7", "xP5xP7", "xP6xP7",
# "rPvdcount",
# "rPvdlfl1", "rPvdlfl1z", "rPvdwdimm",
# "rPvdwdimmz", "rPvdwddel", "rPvdwddelz",
# "rPvdexf7", "rPvdexf7z", "rPvdori",
# "rPvdlfl2", "rPvdlfl3", "rPvdsevens",
# "rPjorm1", "rPjorm10", "rPjorm11",
# "rPjorm12", "rPjorm13", "rPjorm14",
# "rPjorm15", "rPjorm16", "rPjorm2",
# "rPjorm3", "rPjorm4", "rPjorm5",
# "rPjorm6", "rPjorm7", "rPjorm8",
# "rPjorm9", "PD554", "rPD554",
# "PD555", "rPD555", "PD556",
# "rPD556", "rPG014", "rPG021",
# "rPG023", "rPG030", "rPG040",
# "rPG041", "rPG044", "rPG047",
# "rPG050", "rPG059", "rPself_concerns")
# foo <- tracker_merged %>%
#   filter(inHRS_16==1) %>%
#   select(all_of(impute_vars))
#
# foo <- foo %>%
#   haven::zap_labels()
#
# md_pattern_foo <- mice::md.pattern(foo)
# md_pattern_foo <- md_pattern_foo %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column( "value") %>%
#   mutate(n = str_sub(value, 2, -1),
#          n = as.numeric(n)) %>%
#   tibble()

ini_16 <- mice::mice(tracker_16, maxit=0)
pred16 <- ini_16$predictorMatrix
pred16[,'HHID'] <- 0
pred16['HHID',] <- 0
pred16[,'PN'] <- 0
pred16['PN',] <- 0
pred16[,'PSUBHH'] <- 0
pred16['PSUBHH',] <- 0
imp_result_16 <- mice::mice(tracker_16, pred = pred16, m = 1, seed = 2 , visitSequence = "monotone", printFlag = TRUE)
completed_16 <- mice::complete(imp_result_16, 1)

ini_18 <- mice::mice(tracker_18, maxit=0)
pred18 <- ini_18$predictorMatrix
pred18[,'HHID'] <- 0
pred18['HHID',] <- 0
pred18[,'PN'] <- 0
pred18['PN',] <- 0
pred18[,'QSUBHH'] <- 0
pred18['QSUBHH',] <- 0
imp_result_18 <- mice::mice(tracker_18, pred = pred18, m = 1, seed = 2 , printFlag = TRUE)
completed_18 <- mice::complete(imp_result_18, 1)

ini_20 <- mice::mice(tracker_20, maxit=0)
pred20 <- ini_20$predictorMatrix
pred20[,'HHID'] <- 0
pred20['HHID',] <- 0
pred20[,'PN'] <- 0
pred20['PN',] <- 0
pred20[,'RSUBHH'] <- 0
pred20['RSUBHH',] <- 0
imp_result_20 <- mice::mice(tracker_20, pred = pred20, m = 1, seed = 2 , printFlag = TRUE)
completed_20 <- mice::complete(imp_result_20, 1)

ini_22 <- mice::mice(tracker_22, maxit=0)
pred22 <- ini_22$predictorMatrix
pred22[,'HHID'] <- 0
pred22['HHID',] <- 0
pred22[,'PN'] <- 0
pred22['PN',] <- 0
pred22[,'SSUBHH'] <- 0
pred22['SSUBHH',] <- 0
imp_result_22 <- mice::mice(tracker_22, pred = pred22, m = 1, seed = 2 , printFlag = TRUE)
completed_22 <- mice::complete(imp_result_22, 1)


completed_16 <- completed_16 %>%
  rename(PD102_imp = PD102) %>%
  select(HHID, PN, PSUBHH, PD102_imp)

hrs16_func <- hrs16_func %>%
  left_join(completed_16, by = c("HHID", "PN", "PSUBHH"))

completed_18 <- completed_18 %>%
  rename(QD102_imp = QD102) %>%
  select(HHID, PN, QSUBHH, QD102_imp)

hrs18_func <- hrs18_func %>%
  left_join(completed_18, by = c("HHID", "PN", "QSUBHH"))

completed_20 <- completed_20 %>%
  rename(RD102_imp = RD102) %>%
  select(HHID, PN, RSUBHH, RD102_imp)

hrs20_func <- hrs20_func %>%
  left_join(completed_20, by = c("HHID", "PN", "RSUBHH"))

completed_22 <- completed_22 %>%
  rename(SD102_imp = SD102) %>%
  select(HHID, PN, SSUBHH, SD102_imp)

hrs22_func <- hrs22_func %>%
  left_join(completed_22, by = c("HHID", "PN", "SSUBHH"))

saveRDS(hrs16_func, here::here("R_objects", "A0_019_hrs16_func.rds"))
saveRDS(hrs18_func, here::here("R_objects", "A0_019_hrs18_func.rds"))
saveRDS(hrs20_func, here::here("R_objects", "A0_019_hrs20_func.rds"))
saveRDS(hrs22_func, here::here("R_objects", "A0_019_hrs22_func.rds"))

