
hrs16_cog <- readRDS(here::here("R_objects", "A0_009_hrs16_cog.rds"))

hrs16_cog <- hrs16_cog %>%
  mutate(
    rPD151 = case_when(PD151==1 ~ 1,  # Month - correct
                       PD151==5 ~ 0,  # incorrect
                       PD151==8 ~ 0,  # don't know
                       PD151==9 ~ 0   # refuse
    ),
    rPD152 = case_when(PD152==1 ~ 1,  # Day - correct
                       PD152==5 ~ 0,  # incorrect
                       PD152==8 ~ 0,  # don't know
                       PD152==9 ~ 0   # refuse
    ),
    rPD153 = case_when(PD153==1 ~ 1,  # Year - correct
                       PD153==5 ~ 0,  # incorrect
                       PD153==8 ~ 0,  # don't know
                       PD153==9 ~ 0   # refuse
    ),
    rPD154 = case_when(PD154==1 ~ 1,  # Day of week - correct
                       PD154==5 ~ 0,  # incorrect
                       PD154==8 ~ 0,  # don't know
                       PD154==9 ~ 0   # refuse
    ),

    rPD155 = case_when(PD155==1 ~ 1,  # Scissors - correct
                       PD155==5 ~ 0,  # incorrect
                       PD155==8 ~ 0,  # don't know
                       PD155==9 ~ 0   # refuse
    ),
    rPD156 = case_when(PD156==1 ~ 1,  # Cactus - correct
                       PD156==5 ~ 0,  # incorrect
                       PD156==8 ~ 0,  # don't know
                       PD156==9 ~ 0   # refuse
    ),
    rPD157 = case_when(PD157==1 ~ 1,  # President - correct
                       PD157==5 ~ 0,  # incorrect
                       PD157==8 ~ 0,  # don't know
                       PD157==9 ~ 0   # refuse
    ),
    rPD158 = case_when(PD158==1 ~ 1,  # Vice-president - correct
                       PD158==5 ~ 0,  # incorrect
                       PD158==8 ~ 0,  # don't know
                       PD158==9 ~ 0   # refuse
    ),
    rPD142 = case_when(PD142==93 ~ 1,  # Serial 7's - 1 - correct
                       PD142==998 ~ 0,  # don't know
                       PD142==999 ~ 0,  # refuse
                       !is.na(PD142) ~ 0   # incorrect number
    ),
    rPD143 = case_when(PD142-7==PD143 ~ 1,  # Serial 7's - 2 - correct
                       PD143==998 ~ 0,  # don't know
                       PD143==999 ~ 0,  # refuse
                       !is.na(PD143) ~ 0   # incorrect number
    ),
    rPD144 = case_when(PD143-7==PD144 ~ 1,  # Serial 7's - 3 - correct
                       PD144==998 ~ 0,  # don't know
                       PD144==999 ~ 0,  # refuse
                       !is.na(PD144) ~ 0   # incorrect number
    ),
    rPD145 = case_when(PD144-7==PD145 ~ 1,  # Serial 7's - 4 - correct
                       PD145==998 ~ 0,  # don't know
                       PD145==999 ~ 0,  # refuse
                       !is.na(PD145) ~ 0   # incorrect number
    ),
    rPD146 = case_when(PD145-7==PD146 ~ 1,  # Serial 7's - 5 - correct
                       PD146==998 ~ 0,  # don't know
                       PD146==999 ~ 0,  # refuse
                       !is.na(PD146) ~ 0   # incorrect number
    ),
    rPD198 = case_when(PD198==98 ~ 1, # Animal errors, considering the 98 to be a missing value
                       is.na(PD198) ~ 1, # Replacing the missing values with the median value
                       TRUE ~ PD198),
    vdcount = case_when(PD124==1 | PD129==1 ~ 1, # Count backwards, 1st or 2nd try - correct
                        PD124==5 ~ 0,  # 1st try - incorrect
                        PD124==6 ~ 0,  # 1st try - wants to start over
                        PD124==9 ~ 0,  # 1st try - refuse
                        PD129==5 ~ 1,  # 2nd try - incorrect
                        PD129==9 ~ 1   # 2nd try - refuse
    ),
    vdlfl1 = PD196 - rPD198, # Animals
    vdlfl1 = case_when(vdlfl1<0 ~ 0, # If number of animals correct is less than number of errors, then 0
                       TRUE ~ vdlfl1),
    vdwdimm = PD174, # Word list, Immedicate recall
    vdwddel = PD184, # Word list, Delayed recall,
    vdexf7 = case_when(PNSSCORE %in% c(996, 997, 999) ~ NA,
                       !is.na(PNSSCORE) ~ PNSSCORE)


  )

hrs16_cog <- hrs16_cog %>%
  rowwise() %>%
  mutate(
    vdori = sum(rPD151, rPD152, rPD153, rPD154, na.rm = "exclude"),
    vdori = case_when(is.na(rPD151) & is.na(rPD152) & is.na(rPD153) & is.na(rPD154) ~ NA,
                      TRUE ~ vdori),
    vdlfl2 = sum(rPD155, rPD156, na.rm = "exclude"),
    vdlfl2 = case_when(is.na(rPD155) & is.na(rPD156) ~ NA,
                       TRUE ~ vdlfl2),
    vdlfl3 = sum(rPD157, rPD158, na.rm = "exclude"),
    vdlfl3 = case_when(is.na(rPD157) & is.na(rPD158) ~ NA,
                       TRUE ~ vdlfl3),
    vdsevens = sum(rPD142, rPD143, rPD144, rPD145, rPD146, na.rm = "exclude"),
    vdsevens = case_when(is.na(rPD142) & is.na(rPD143) & is.na(rPD144) & is.na(rPD145) & is.na(rPD146) ~ NA,
                      TRUE ~ vdsevens)
  ) %>%
  ungroup()





hrs16_cog <- hrs16_cog %>%
  labelled::set_variable_labels(rPD151 = "Month") %>%
  labelled::set_value_labels(rPD151 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD152 = "Day") %>%
  labelled::set_value_labels(rPD152 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD153 = "Year") %>%
  labelled::set_value_labels(rPD153 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD154 = "Day of week") %>%
  labelled::set_value_labels(rPD154 = c("Correct" = 1, "Incorrect" = 0)) %>%

  labelled::set_variable_labels(rPD155 = "Scissors") %>%
  labelled::set_value_labels(rPD155 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD156 = "Cactus") %>%
  labelled::set_value_labels(rPD156 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD157 = "President") %>%
  labelled::set_value_labels(rPD157 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD158 = "Vice-president") %>%
  labelled::set_value_labels(rPD158 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD142 = "Serial 7's - 1") %>%
  labelled::set_value_labels(rPD142 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD143 = "Serial 7's - 2") %>%
  labelled::set_value_labels(rPD143 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD144 = "Serial 7's -3") %>%
  labelled::set_value_labels(rPD144 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD145 = "Serial 7's - 4") %>%
  labelled::set_value_labels(rPD145 = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(rPD146 = "Serial 7's - 5") %>%
  labelled::set_value_labels(rPD146 = c("Correct" = 1, "Incorrect" = 0)) %>%

  labelled::set_variable_labels(vdori = "Orientation to Time - number correct") %>%
  labelled::set_variable_labels(vdlfl2 = "Object naming - Scissors, cactus") %>%
  labelled::set_variable_labels(vdlfl3 = "Naming - President, Vice-president") %>%
  labelled::set_variable_labels(vdcount = "Count backwards from 20") %>%
  labelled::set_value_labels(vdcount = c("Correct" = 1, "Incorrect" = 0)) %>%
  labelled::set_variable_labels(vdsevens = "Serial 7's - Number correct") %>%
  labelled::set_variable_labels(vdlfl1 = "Animal naming (correct - errors)") %>%
  labelled::set_variable_labels(vdwdimm = "Word recall - Immediate") %>%
  labelled::set_variable_labels(vdwddel = "Word recall - Delayed") %>%
  labelled::set_variable_labels(vdexf7  = "Number series")

hrs16_cog_notes <- tribble(~v, ~notes,
  "vdori", "This is the number of correct responses to the orientation to time items - month/day/year/day of week.  It ranges from 0 - 4.",
  "vdlfl2", "This is the number of correct responses to the object naming items - scissors and cactus.  It ranges from 0 - 2.",
  "vdlfl3", "This is the number of correct responses to the president/vice-president naming items.  It ranges from 0 - 2.",
  "vdcount", "This is an indicator for whether the count backwards from 20 item was correct on either the first or second try.",
  "vdsevens", "This is the number of correct responses to the serial sevens subtraction items.  It ranges from 0 - 5.",
  "vdlfl1", "This is the score on the animal naming item. It is calculated as the number of animals correctly named minus the errors. Missing values on errors are set to 1 (the median value of errors).",
  "vdwdimm", "This is the number of words correctly recalled on immediate recall.  It ranges from 0 - 10.",
  "vdwddel", "This is the number of words correctly recalled on delayed recall.  It ranges from 0 - 10.",
  "vdexf7", "This is the score on the number series test."
                           )



saveRDS(hrs16_cog,       here::here("R_objects", "A0_015_hrs16_cog.rds"))
saveRDS(hrs16_cog_notes, here::here("R_objects", "A0_015_hrs16_cog_notes.rds"))
