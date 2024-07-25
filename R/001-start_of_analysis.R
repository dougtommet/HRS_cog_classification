library(tidyverse)


dropbox_path <- fs::path_home("Library", "CloudStorage", "Dropbox")
hcap23_path <- fs::path(dropbox_path, "work", "HCAP23")
hcap23_data_path <- fs::path(hcap23_path, "POSTED", "DATA", "SOURCE")

hrs_path <- fs::path(dropbox_path, "work", "HRS")
hrs_data_path <- fs::path(hrs_path, "POSTED", "DATA", "SOURCE")

hc16hp_r <- haven::read_dta(fs::path(hcap23_data_path, "HC16", "HC16sta", "hc16hp_r.dta"))

h16a_r <- haven::read_sav(fs::path(hrs_data_path, "2016", "H16A_R.sav"))
h16c_r <- haven::read_sav(fs::path(hrs_data_path, "2016", "H16C_R.sav"))
h16d_r <- haven::read_sav(fs::path(hrs_data_path, "2016", "H16D_R.sav"))


hrs16_cog <- h16d_r %>%
  select(HHID, PN, PSUBHH,
         # starts_with("PD182M"), # using PD174 instead
         PD174,
         # starts_with("PD183M"), # using PD184 instead
         PD184,
         PD151, PD152, PD153, PD154,
         PD124, PD129,
         PD155, PD156,
         PD157, PD158,
         PD142, PD143, PD144, PD145, PD146,
         PD196, PNSSCORE)



hrs16_func <- h16d_r %>%
  select(HHID, PN, PSUBHH,
         PD102,
         PD502, PD505, PD506, PD507, PD508, PD509, PD510,
         PD511, PD512, PD513, PD514, PD515, PD516, PD517, PD518, PD519, PD520,
         PD521, PD522, PD523, PD524, PD525, PD526, PD527, PD528, PD529, PD530,
         PD531, PD532, PD533, PD534, PD535, PD536, PD537, PD538, PD539, PD540,
         PD541, PD542, PD543, PD544, PD545, PD546, PD547, PD548, PD549, PD550,
         PD551, PD552, PD553)

# From dropbox/work/pitch/posted/analysis/hrs coda/extract hrscogdata170322.do
# callhrs_lg , datadrive($d/Laura/PITCH/HRS) substublist(2014) h(h)
# ids(HHID PN) newmetadata varstopull(ORC104
#   OD104 OD124 OD129 OD142 OD143 OD144 OD145 OD146 OD184 OD174 OD151
#   OD152 OD153 OD154 OD155 OD156 OD157 OD158
#   OD178 OD179 OD180 OD196 OD198 OD250 OD251 OD252 OD253 OD254 OD255 OD256
#   OD257 OD258 OD259 OD260 OD261 OD262 OD263 OD264   OD265
#   OD270 OD271 OD272 OD273 OD274 OD275 OD276 OD277 OD278
#   OD279 OD280 OD281 OD282 OD283 OD284 OVESCORE OVESCORESE
#   OA019 OB015 OB014 OB028 OX060_R OB091M	);

# These are the items from the list above that weren't selected
# OD104
#   OD198 OD250 OD251 OD252 OD253 OD254 OD255 OD256
#   OD257 OD258 OD259 OD260 OD261 OD262 OD263 OD264   OD265
#   OD270 OD271 OD272 OD273 OD274 OD275 OD276 OD277 OD278
#   OD279 OD280 OD281 OD282 OD283 OD284 OVESCORE OVESCORESE

hrs16_cog <- hrs16_cog %>%
  mutate(rPD151 = case_when(PD151==1 ~ 0,  # Month - correct
                            PD151==5 ~ 1,  # incorrect
                            PD151==8 ~ 1,  # don't know
                            PD151==9 ~ 1   # refuse
                            ),
         rPD152 = case_when(PD152==1 ~ 0,  # Day - correct
                            PD152==5 ~ 1,  # incorrect
                            PD152==8 ~ 1,  # don't know
                            PD152==9 ~ 1   # refuse
         ),
         rPD153 = case_when(PD153==1 ~ 0,  # Year - correct
                            PD153==5 ~ 1,  # incorrect
                            PD153==8 ~ 1,  # don't know
                            PD153==9 ~ 1   # refuse
         ),
         rPD154 = case_when(PD154==1 ~ 0,  # Day of week - correct
                            PD154==5 ~ 1,  # incorrect
                            PD154==8 ~ 1,  # don't know
                            PD154==9 ~ 1   # refuse
         ),
         rPD124 = case_when(PD124==1 ~ 0,  # Count backwards, 1st try - correct
                            PD124==5 ~ 1,  # incorrect
                            PD124==6 ~ 1,  # wants to start over
                            PD124==9 ~ 1   # refuse
         ),
         rPD129 = case_when(PD129==1 ~ 0,  # Count backwards, 2nd try - correct
                            PD129==5 ~ 1,  # incorrect
                            PD129==6 ~ 1,  # wants to start over
                            PD129==9 ~ 1   # refuse
         ),
         rPD155 = case_when(PD155==1 ~ 0,  # Scissors - correct
                            PD155==5 ~ 1,  # incorrect
                            PD155==8 ~ 1,  # don't know
                            PD155==9 ~ 1   # refuse
         ),
         rPD156 = case_when(PD156==1 ~ 0,  # Cactus - correct
                            PD156==5 ~ 1,  # incorrect
                            PD156==8 ~ 1,  # don't know
                            PD156==9 ~ 1   # refuse
         ),
         rPD157 = case_when(PD157==1 ~ 0,  # President - correct
                            PD157==5 ~ 1,  # incorrect
                            PD157==8 ~ 1,  # don't know
                            PD157==9 ~ 1   # refuse
         ),
         rPD158 = case_when(PD158==1 ~ 0,  # Vice-president - correct
                            PD158==5 ~ 1,  # incorrect
                            PD158==8 ~ 1,  # don't know
                            PD158==9 ~ 1   # refuse
         ),
         rPD142 = case_when(PD142==93 ~ 0,  # Serial 7's - 1 - correct
                            PD142==998 ~ 1,  # don't know
                            PD142==999 ~ 1,  # refuse
                            !is.na(PD142) ~ 1   # incorrect number
         ),
         rPD143 = case_when(PD143==86 ~ 0,  # Serial 7's - 2 - correct
                            PD143==998 ~ 1,  # don't know
                            PD143==999 ~ 1,  # refuse
                            !is.na(PD143) ~ 1   # incorrect number
         ),
         rPD144 = case_when(PD144==79 ~ 0,  # Serial 7's - 3 - correct
                            PD144==998 ~ 1,  # don't know
                            PD144==999 ~ 1,  # refuse
                            !is.na(PD144) ~ 1   # incorrect number
         ),
         rPD145 = case_when(PD145==72 ~ 0,  # Serial 7's - 4 - correct
                            PD145==998 ~ 1,  # don't know
                            PD145==999 ~ 1,  # refuse
                            !is.na(PD145) ~ 1   # incorrect number
         ),
         rPD146 = case_when(PD146==65 ~ 0,  # Serial 7's - 5 - correct
                            PD146==998 ~ 1,  # don't know
                            PD146==999 ~ 1,  # refuse
                            !is.na(PD146) ~ 1   # incorrect number
         ),
         rPD196 = PD196,
         rPD174 = PD174,
         rPD184 = PD184

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


hrs16_cog %>%
  select(starts_with("r")) %>%
  labelled::to_factor() %>%
  gtsummary::tbl_summary(
    type = c(starts_with("r")) ~ "categorical"
  )


