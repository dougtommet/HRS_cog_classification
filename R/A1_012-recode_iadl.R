
hrs16_iadl <- readRDS(fs::path(r_objects_folder, "010_hrs16_iadl.rds"))

hrs16_iadl %>%
  select(-HHID, -PN, -PSUBHH) %>%
  gtsummary::tbl_summary()

hrs16_iadl <- hrs16_iadl %>%
  mutate(rPG014 = case_when(PG014==1 ~ 1,
                            PG014==5 ~ 0,
                            PG014==6 ~ 1,
                            PG014==7 ~ NA,
                            PG014==8 ~ NA,
                            PG014==9 ~ NA),
         rPG021 = case_when(PG021==1 ~ 1,
                            PG021==5 ~ 0,
                            PG021==6 ~ 1,
                            PG021==7 ~ NA,
                            PG021==8 ~ NA,
                            PG021==9 ~ NA),
         rPG023 = case_when(PG023==1 ~ 1,
                            PG023==5 ~ 0,
                            PG023==6 ~ 1,
                            PG023==7 ~ NA,
                            PG023==8 ~ NA,
                            PG023==9 ~ NA),
         rPG030 = case_when(PG030==1 ~ 1,
                            PG030==5 ~ 0,
                            PG030==6 ~ 1,
                            PG030==7 ~ NA,
                            PG030==8 ~ NA,
                            PG030==9 ~ NA),
         rPG040 = case_when(PG040==1 ~ 1,
                            PG040==5 ~ 0,
                            PG040==6 ~ 1,
                            PG040==7 ~ NA,
                            PG040==8 ~ NA,
                            PG040==9 ~ NA),
         rPG041 = case_when(PG041==1 ~ 1,
                            PG041==5 ~ 0,
                            PG041==6 ~ 1,
                            PG041==7 ~ NA,
                            PG041==8 ~ NA,
                            PG041==9 ~ NA),
         rPG044 = case_when(PG044==1 ~ 1,
                            PG044==5 ~ 0,
                            PG044==6 ~ 1,
                            PG044==7 ~ NA,
                            PG044==8 ~ NA,
                            PG044==9 ~ NA),
         rPG047 = case_when(PG047==1 ~ 1,
                            PG047==5 ~ 0,
                            PG047==6 ~ 1,
                            PG047==7 ~ NA,
                            PG047==8 ~ NA,
                            PG047==9 ~ NA),
         rPG050 = case_when(PG050==1 ~ 1,
                            PG050==5 ~ 0,
                            PG050==6 ~ 1,
                            # PG050==7 ~ NA,
                            PG050==8 ~ NA,
                            PG050==9 ~ NA,
                            PG050==7 & PG051==1 ~ 1,
                            PG050==7 & PG051==5 ~ 0,
                            PG050==7 & PG051==8 ~ NA
                            ),
         rPG059 = case_when(PG059==1 ~ 1,
                            PG059==5 ~ 0,
                            PG059==6 ~ 1,
                            PG059==7 ~ NA,
                            PG059==8 ~ NA,
                            PG059==9 ~ NA)
         ) %>%
  rowwise() %>%
  mutate(iadl_imp = sum(rPG014, rPG021, rPG023, rPG030, rPG040, rPG041, rPG044, rPG047, rPG050, rPG059, na.rm=TRUE)) %>%
  ungroup()

# goo <- QSPtools::checkvar(hrs16_iadl, iadl_imp, rPG014, rPG021, rPG023, rPG030, rPG040, rPG041, rPG044, rPG047, rPG050, rPG059)
# QSPtools::checkvar(hrs16_iadl, rPG050, PG050, PG051)

hrs16_iadl <- hrs16_iadl %>%
  labelled::set_variable_labels(rPG014 = "ADL: Difficulty Dressing") %>%
  labelled::set_value_labels(rPG014 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(rPG021 = "ADL: Difficulty Bathing") %>%
  labelled::set_value_labels(rPG021 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(rPG023 = "ADL: Difficulty Eating") %>%
  labelled::set_value_labels(rPG023 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(rPG030 = "ADL: Difficulty Using Toilet") %>%
  labelled::set_value_labels(rPG030 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(rPG040 = "IADL: Difficulty Using Maps") %>%
  labelled::set_value_labels(rPG040 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(rPG041 = "IADL: Difficulty Meal Prep") %>%
  labelled::set_value_labels(rPG041 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(rPG044 = "IADL: Difficulty Grocery Shopping") %>%
  labelled::set_value_labels(rPG044 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(rPG047 = "IADL: Difficulty Making Phone Calls") %>%
  labelled::set_value_labels(rPG047 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(rPG050 = "IADL: Difficulty Taking Meds") %>%
  labelled::set_value_labels(rPG050 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(rPG059 = "IADL: Difficulty Managing Money") %>%
  labelled::set_value_labels(rPG059 = c("Impaired" = 1, "Not impaired" = 0)) %>%
  labelled::set_variable_labels(iadl_imp = "Sum of ADL/IADL impairments")

saveRDS(hrs16_iadl, fs::path(r_objects_folder, "012_hrs16_iadl.rds"))



