


hrs16_cog <- readRDS(fs::path(r_objects_folder, "010_hrs16_cog.rds"))


hrs16_cog <- hrs16_cog %>%
  mutate(
    # agegroup = car::recode(PA019, "lo:64 = 0; 65:69 = 1; 70:74 = 2; 75:79 = 3; 80:84 = 4; 85:89 = 5; 90:94 = 6; 95:hi = 7;"),
    rage = PA019,
    # This was from RNJ's github code, but it is not needed
    # rage75 = case_when(!is.na(PA019) ~ 75),
    # ragec75 = case_when(!is.na(PA019) ~ rage - rage75),
    # raged75 = case_when(!is.na(PA019) ~ rage>rage75),
    # ragei75 = ragec75 * raged75,
    #
    # rage85 = case_when(!is.na(PA019) ~ 85),
    # ragec85 = case_when(!is.na(PA019) ~ rage - rage85),
    # raged85 = case_when(!is.na(PA019) ~ rage>rage85),
    # ragei85 = ragec85 * raged85,
    #
    # agegroup = car::recode(rage, "lo:74 = 1; 75:84 = 2; 84:hi = 3; ")
  )


# in file 015-Call-data-CORE-respondent.do, rnj manually recoded education for these five participants
hrs16_cog <- hrs16_cog %>%
  mutate(SCHLYRS = case_when(HHID==134189 & PN==020 ~ 14,
                             HHID==502334 & PN==010 ~ 12,
                             HHID==502607 & PN==010 ~ 10,
                             HHID==905576 & PN==010 ~ 16,
                             HHID==906962 & PN==010 ~ 12,
                             TRUE ~ SCHLYRS),
         SCHLYRS = case_when(SCHLYRS==99 ~ NA,
                             TRUE ~ SCHLYRS))

# This was from RNJ's github code, but is not needed
# hrs16_cog <- hrs16_cog %>%
#   mutate(
#     edcat = car::recode(SCHLYRS, "0:8 = 1; 9:11 = 2; 12:12 = 3; 13:15 = 4; 16:16 = 5; 16:hi = 6"),
#   )


hrs16_cog <- hrs16_cog %>%
  mutate(female = case_when(GENDER==2 ~ 1,
                            GENDER==1 ~ 0))

hrs16_cog <- hrs16_cog %>%
  mutate(black = case_when(RACE ==2 & !(HISPANIC %in% c(1, 2, 3)) ~ 1,
                           RACE ==2 & (HISPANIC %in% c(1, 2, 3)) ~ 0,
                           RACE %in% c(0, 1, 7) ~ 0))

hrs16_cog <- hrs16_cog %>%
  mutate(hisp = case_when(HISPANIC %in% c(1, 2, 3) ~ 1,
                          HISPANIC %in% c(5, 0) ~ 0))


hrs16_cog <- hrs16_cog %>%
  # labelled::set_variable_labels(agegroup = "Age group") %>%
  # labelled::set_value_labels(agegroup = c("Under 75" = 1, "75 - 84" = 2, "85 and over" = 3)) %>%

  # labelled::set_variable_labels(edcat = "Education level") %>%
  # labelled::set_value_labels(edcat = c("schlyrs_0-8" = 1, "schlyrs_9-11" = 2, "schlyrs_12" = 3, "schlyrs_13-15" = 4, "schlyrs_16" = 5, "schlyrs_17up" = 6)) %>%

  labelled::set_variable_labels(female = "Female from trk2022tr_r") %>%
  labelled::set_value_labels(female = c("Female" = 1, "Male" = 0)) %>%

  labelled::set_variable_labels(black = "Black or African-American (not Hispanic) from trk2022tr_r") %>%
  labelled::set_value_labels(black = c("Black" = 1, "Non-Black" = 0)) %>%

  labelled::set_variable_labels(hisp = "Hispanic from trk2022tr_r") %>%
  labelled::set_value_labels(hisp = c("Hispanic" = 1, "Non-Hispanic" = 0))

saveRDS(hrs16_cog, fs::path(r_objects_folder, "011_hrs16_cog.rds"))

