
hrs16_cog <- readRDS(fs::path(r_objects_folder, "010_hrs16_cog.rds"))

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
    rPD198 = case_when(PD198==98 ~ NA, # Animal errors
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


# hrs16_cog <- hrs16_cog %>%
#   mutate(rPD151 = case_when(PD151==1 ~ 0,  # Month - correct
#                             PD151==5 ~ 1,  # incorrect
#                             PD151==8 ~ 1,  # don't know
#                             PD151==9 ~ 1   # refuse
#   ),
#   rPD152 = case_when(PD152==1 ~ 0,  # Day - correct
#                      PD152==5 ~ 1,  # incorrect
#                      PD152==8 ~ 1,  # don't know
#                      PD152==9 ~ 1   # refuse
#   ),
#   rPD153 = case_when(PD153==1 ~ 0,  # Year - correct
#                      PD153==5 ~ 1,  # incorrect
#                      PD153==8 ~ 1,  # don't know
#                      PD153==9 ~ 1   # refuse
#   ),
#   rPD154 = case_when(PD154==1 ~ 0,  # Day of week - correct
#                      PD154==5 ~ 1,  # incorrect
#                      PD154==8 ~ 1,  # don't know
#                      PD154==9 ~ 1   # refuse
#   ),
#   rPD124 = case_when(PD124==1 ~ 0,  # Count backwards, 1st try - correct
#                      PD124==5 ~ 1,  # incorrect
#                      PD124==6 ~ 1,  # wants to start over
#                      PD124==9 ~ 1   # refuse
#   ),
#   rPD129 = case_when(PD129==1 ~ 0,  # Count backwards, 2nd try - correct
#                      PD129==5 ~ 1,  # incorrect
#                      PD129==6 ~ 1,  # wants to start over
#                      PD129==9 ~ 1   # refuse
#   ),
#   rPD155 = case_when(PD155==1 ~ 0,  # Scissors - correct
#                      PD155==5 ~ 1,  # incorrect
#                      PD155==8 ~ 1,  # don't know
#                      PD155==9 ~ 1   # refuse
#   ),
#   rPD156 = case_when(PD156==1 ~ 0,  # Cactus - correct
#                      PD156==5 ~ 1,  # incorrect
#                      PD156==8 ~ 1,  # don't know
#                      PD156==9 ~ 1   # refuse
#   ),
#   rPD157 = case_when(PD157==1 ~ 0,  # President - correct
#                      PD157==5 ~ 1,  # incorrect
#                      PD157==8 ~ 1,  # don't know
#                      PD157==9 ~ 1   # refuse
#   ),
#   rPD158 = case_when(PD158==1 ~ 0,  # Vice-president - correct
#                      PD158==5 ~ 1,  # incorrect
#                      PD158==8 ~ 1,  # don't know
#                      PD158==9 ~ 1   # refuse
#   ),
#   rPD142 = case_when(PD142==93 ~ 0,  # Serial 7's - 1 - correct
#                      PD142==998 ~ 1,  # don't know
#                      PD142==999 ~ 1,  # refuse
#                      !is.na(PD142) ~ 1   # incorrect number
#   ),
#   rPD143 = case_when(PD143==86 ~ 0,  # Serial 7's - 2 - correct
#                      PD143==998 ~ 1,  # don't know
#                      PD143==999 ~ 1,  # refuse
#                      !is.na(PD143) ~ 1   # incorrect number
#   ),
#   rPD144 = case_when(PD144==79 ~ 0,  # Serial 7's - 3 - correct
#                      PD144==998 ~ 1,  # don't know
#                      PD144==999 ~ 1,  # refuse
#                      !is.na(PD144) ~ 1   # incorrect number
#   ),
#   rPD145 = case_when(PD145==72 ~ 0,  # Serial 7's - 4 - correct
#                      PD145==998 ~ 1,  # don't know
#                      PD145==999 ~ 1,  # refuse
#                      !is.na(PD145) ~ 1   # incorrect number
#   ),
#   rPD146 = case_when(PD146==65 ~ 0,  # Serial 7's - 5 - correct
#                      PD146==998 ~ 1,  # don't know
#                      PD146==999 ~ 1,  # refuse
#                      !is.na(PD146) ~ 1   # incorrect number
#   ),
#   rPD196 = car::recode(PD196, "lo:9 = 9; 10:11 = 8; 12:13 = 7; 14:15 = 6;
#                        16:17 = 5; 18:19 = 4; 20:21 = 3; 22:23 = 2;
#                        24:27 = 1; 28:hi = 0"),
#   rPD174 = car::recode(PD174, "0 = 9; 1 = 8; 2 = 7; 3 = 6;
#                        4 = 5; 5 = 4; 6:7 = 3; 8 = 2;
#                        9 = 1; 10 = 0"),
#   rPD184 = car::recode(PD184, "0 = 9; 1 = 8; 2 = 7; 3 = 6;
#                        4 = 5; 5 = 4; 6:7 = 3; 8 = 2;
#                        9 = 1; 10 = 0")
#
#   )
# hrs16_cog <- hrs16_cog %>%
#   rowwise() %>%
#   mutate(n_miss = sum(is.na(rPD142), is.na(rPD143), is.na(rPD144), is.na(rPD145), is.na(rPD146)),
#          rPD147_tmp = sum(rPD142, rPD143, rPD144, rPD145, rPD146, na.rm = "exclude"),
#          rPD147 = case_when(n_miss==5 ~ NA,
#                             TRUE ~ rPD147_tmp * (5/(5-n_miss)))
#          ) %>%
#   ungroup() %>%
#   select(-n_miss, -rPD147_tmp)
#



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
  "vdlfl1", "This is the score on the animal naming item. It is calculated as the number of animals correctly named minus the errors.",
  "vdwdimm", "This is the number of words correctly recalled on immediate recall.  It ranges from 0 - 10.",
  "vdwddel", "This is the number of words correctly recalled on delayed recall.  It ranges from 0 - 10.",
  "vdexf7", "This is the score on the number series test.  I am unfamiliar with this test."
                           )

# # This was from before deciding to use PD174
# hrs16_cog <- hrs16_cog %>%
#   mutate(
#     rPD182M1 = case_when(PD182M1<=40 ~ 1, # correct
#                          PD182M1>=51 & PD182M1<=63 ~ 0, # incorrect
#                          PD182M1==96 ~ 0, # None remembered
#                          PD182M1==98 ~ 0, # Don't know
#                          PD182M1==99 ~ 0, # Refused
#     ),
#     rPD182M2 = case_when(PD182M2<=40 ~ 1, # correct
#                          PD182M2>=51 & PD182M2<=63 ~ 0, # incorrect
#                          PD182M2==96 ~ 0, # None remembered
#                          PD182M2==98 ~ 0, # Don't know
#                          PD182M2==99 ~ 0, # Refused
#     ),
#     rPD182M3 = case_when(PD182M3<=40 ~ 1, # correct
#                          PD182M3>=51 & PD182M3<=63 ~ 0, # incorrect
#                          PD182M3==96 ~ 0, # None remembered
#                          PD182M3==98 ~ 0, # Don't know
#                          PD182M3==99 ~ 0, # Refused
#     ),
#     rPD182M4 = case_when(PD182M4<=40 ~ 1, # correct
#                          PD182M4>=51 & PD182M4<=63 ~ 0, # incorrect
#                          PD182M4==96 ~ 0, # None remembered
#                          PD182M4==98 ~ 0, # Don't know
#                          PD182M4==99 ~ 0, # Refused
#     ),
#     rPD182M5 = case_when(PD182M5<=40 ~ 1, # correct
#                          PD182M5>=51 & PD182M5<=63 ~ 0, # incorrect
#                          PD182M5==96 ~ 0, # None remembered
#                          PD182M5==98 ~ 0, # Don't know
#                          PD182M5==99 ~ 0, # Refused
#     ),
#     rPD182M6 = case_when(PD182M6<=40 ~ 1, # correct
#                          PD182M6>=51 & PD182M6<=63 ~ 0, # incorrect
#                          PD182M6==96 ~ 0, # None remembered
#                          PD182M6==98 ~ 0, # Don't know
#                          PD182M6==99 ~ 0, # Refused
#     ),
#     rPD182M7 = case_when(PD182M7<=40 ~ 1, # correct
#                          PD182M7>=51 & PD182M7<=63 ~ 0, # incorrect
#                          PD182M7==96 ~ 0, # None remembered
#                          PD182M7==98 ~ 0, # Don't know
#                          PD182M7==99 ~ 0, # Refused
#     ),
#     rPD182M8 = case_when(PD182M8<=40 ~ 1, # correct
#                          PD182M8>=51 & PD182M8<=63 ~ 0, # incorrect
#                          PD182M8==96 ~ 0, # None remembered
#                          PD182M8==98 ~ 0, # Don't know
#                          PD182M8==99 ~ 0, # Refused
#     ),
#     rPD182M9 = case_when(PD182M9<=40 ~ 1, # correct
#                          PD182M9>=51 & PD182M9<=63 ~ 0, # incorrect
#                          PD182M9==96 ~ 0, # None remembered
#                          PD182M9==98 ~ 0, # Don't know
#                          PD182M9==99 ~ 0, # Refused
#     ),
#     rPD182M10 = case_when(PD182M10<=40 ~ 1, # correct
#                           PD182M10>=51 & PD182M10<=63 ~ 0, # incorrect
#                           PD182M10==96 ~ 0, # None remembered
#                           PD182M10==98 ~ 0, # Don't know
#                           PD182M10==99 ~ 0, # Refused
#     ),
#     rPD182M11 = case_when(PD182M11<=40 ~ 1, # correct
#                           PD182M11>=51 & PD182M11<=63 ~ 0, # incorrect
#                           PD182M11==96 ~ 0, # None remembered
#                           PD182M11==98 ~ 0, # Don't know
#                           PD182M11==99 ~ 0, # Refused
#     ),
#     rPD182M12 = case_when(PD182M12<=40 ~ 1, # correct
#                           PD182M12>=51 & PD182M12<=63 ~ 0, # incorrect
#                           PD182M12==96 ~ 0, # None remembered
#                           PD182M12==98 ~ 0, # Don't know
#                           PD182M12==99 ~ 0, # Refused
#     ),
#     rPD182M13 = case_when(PD182M13<=40 ~ 1, # correct
#                           PD182M13>=51 & PD182M13<=63 ~ 0, # incorrect
#                           PD182M13==96 ~ 0, # None remembered
#                           PD182M13==98 ~ 0, # Don't know
#                           PD182M13==99 ~ 0, # Refused
#     ),
#     rPD182M14 = case_when(PD182M14<=40 ~ 1, # correct
#                           PD182M14>=51 & PD182M14<=63 ~ 0, # incorrect
#                           PD182M14==96 ~ 0, # None remembered
#                           PD182M14==98 ~ 0, # Don't know
#                           PD182M14==99 ~ 0, # Refused
#     ),
#     rPD182M15 = case_when(PD182M15<=40 ~ 1, # correct
#                           PD182M15>=51 & PD182M15<=63 ~ 0, # incorrect
#                           PD182M15==96 ~ 0, # None remembered
#                           PD182M15==98 ~ 0, # Don't know
#                           PD182M15==99 ~ 0, # Refused
#     ),
#     rPD182M16 = case_when(PD182M16<=40 ~ 1, # correct
#                           PD182M16>=51 & PD182M16<=63 ~ 0, # incorrect
#                           PD182M16==96 ~ 0, # None remembered
#                           PD182M16==98 ~ 0, # Don't know
#                           PD182M16==99 ~ 0, # Refused
#     ),
#     rPD182M17 = case_when(PD182M17<=40 ~ 1, # correct
#                           PD182M17>=51 & PD182M17<=63 ~ 0, # incorrect
#                           PD182M17==96 ~ 0, # None remembered
#                           PD182M17==98 ~ 0, # Don't know
#                           PD182M17==99 ~ 0, # Refused
#     ),
#     rPD182M18 = case_when(PD182M18<=40 ~ 1, # correct
#                           PD182M18>=51 & PD182M18<=63 ~ 0, # incorrect
#                           PD182M18==96 ~ 0, # None remembered
#                           PD182M18==98 ~ 0, # Don't know
#                           PD182M18==99 ~ 0, # Refused
#     )
#   )
# hrs16_cog <- hrs16_cog %>%
#   rowwise() %>%
#   mutate(rPD182 = sum(rPD182M1, rPD182M2, rPD182M3, rPD182M4, rPD182M5,
#                       rPD182M6, rPD182M7, rPD182M8, rPD182M9, rPD182M10,
#                       rPD182M11, rPD182M12, rPD182M13, rPD182M14, rPD182M15,
#                       rPD182M16, rPD182M17, na.rm = "exclude")
#            ) %>%
#   ungroup()

saveRDS(hrs16_cog, fs::path(r_objects_folder, "015_hrs16_cog.rds"))
saveRDS(hrs16_cog_notes, fs::path(r_objects_folder, "015_hrs16_cog_notes.rds"))
