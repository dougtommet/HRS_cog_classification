

tracker_demo    <- readRDS(here::here("R_objects", "A0_009_tracker_demo.rds"))
hc16hp_r        <- readRDS(here::here("R_objects", "A0_009_hc16hp_r.rds"))
hcap_validation <- readRDS(here::here("R_objects", "A0_009_hcap_validation.rds"))

tracker_demo <- tracker_demo %>%
  left_join(hc16hp_r, by = c("HHID", "PN"))  %>%
  left_join(hcap_validation, by = c("HHID", "PN"))


# Nonzero weight indicator
# in HCAP sample: HCAP16RESP 0=No participation, 1=Informant only, 2=Respondent only, 3=Both
# Age65+ indicator
# ID variable for mplus
# SECU variable for mplus
tracker_demo <- tracker_demo %>%
  mutate(inHRS_16 = case_when(PIWTYPE==1 ~ 1,
                              PIWTYPE!=1 ~ 0),
         nonzeroweight = case_when(is.na(PWGTR) ~ 0,
                                   PWGTR > 0 ~ 1,
                                   PWGTR==0 ~ 0),
         inHCAP = case_when(HCAP16RESP %in% c(1, 2, 3) ~ 1,
                            TRUE ~ 0),
         inSamplingFrame = case_when(nonzeroweight==1 & inHCAP==1 ~ 1,
                                     nonzeroweight==0 & inHCAP==1 ~ 2 ,
                                     nonzeroweight==1 & inHCAP==0 ~ 3 ,
                                     nonzeroweight==0 & inHCAP==0 ~ 4 ),
         inSamplingFrame = labelled(inSamplingFrame, labels = c("In HCAP, has weight" = 1,
                                                                 "In HCAP, no weight" = 2,
                                                                 "Has weight, not in HCAP" = 3,
                                                                 "No weight, not in HCAP" = 4
                                                                 )
                                     ),
         age65up = case_when(PA019 >= 65  ~ 1,
                             PA019 < 65 ~ 0,
                             PAGE >=65 ~ 1,
                             PAGE < 65 ~ 0,
                             is.na(PA019) ~ NA_real_),
         id = str_c(HHID, PN),
         SECU_mplus = 10*STRATUM+SECU
  )
attr(tracker_demo$inHRS_16, "label") <- "In HRS 2016"
attr(tracker_demo$nonzeroweight, "label") <- "Has positive 2016 sampling weight"
attr(tracker_demo$inHCAP, "label") <- "In HCAP 2016"
attr(tracker_demo$inSamplingFrame, "label") <- "Weight and HCAP indicator"
attr(tracker_demo$age65up, "label") <- "Age 65+"
attr(tracker_demo$id, "label") <- "ID: HHID+PN"
attr(tracker_demo$SECU_mplus, "label") <- "SECU for Mplus"




saveRDS(tracker_demo, here::here("R_objects", "A0_010_tracker_demo.rds"))
