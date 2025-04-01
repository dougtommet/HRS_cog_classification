


hrs16_cog <- readRDS(fs::path(r_objects_folder, "025_hrs16_cog.rds"))
hrs16_cog_norm <- readRDS( fs::path(r_objects_folder, "025_hrs16_cog_norm.rds"))
hrs16_iadl <- readRDS(fs::path(r_objects_folder, "012_hrs16_iadl.rds"))
hrs16_func <- readRDS(fs::path(r_objects_folder, "010_hrs16_func.rds"))

hrs16_func <- hrs16_func  %>%
  mutate(self_concerns = case_when(PD102==3 ~ 1,
                                   PD102 %in% c(1, 2) ~ 0)) %>%
  select(HHID, PN, self_concerns)


quantile(hrs16_cog_norm$TF, probs = c(.15, .35), na.rm=TRUE)
# Not able to get the same centiles of 36.0 and 43.3 as in the google presentations

foo <- hrs16_cog_norm %>%
  left_join(hrs16_iadl, by = c("HHID" = "HHID", "PN" = "PN")) %>%
  left_join(hrs16_func, by = c("HHID" = "HHID", "PN" = "PN"))


foo <- foo %>%
  mutate(cog_imp = case_when(TF <36.0 ~ 2,
                             TF < 43.3 ~ 1,
                             !is.na(TF) ~ 0))
gtsummary::tbl_summary(foo, include = c(cog_imp),
                       type = c(cog_imp) ~ "categorical")
table(foo$cog_imp)

foo_2 <- foo %>%
  filter(cog_imp==2)

gtsummary::tbl_summary(foo_2, include = c(iadl_imp),
                       type = c(iadl_imp) ~ "categorical")
# HCAP sample: 375/591 = 63%
# The HRS IADL cutpoints that get the closet: 1+ 24/124 = 19%

foo_1 <- foo %>%
  filter(cog_imp==1)

gtsummary::tbl_summary(foo_1, include = c(iadl_imp),
                       type = c(iadl_imp) ~ "categorical")
# HCAP sample: 556/726 = 75%
# The HRS IADL cutpoints that get the closet: 1+ 60/317 = 19%


#########################
# apply algorithm in HRS

hrs <- hrs16_cog %>%
  left_join(hrs16_iadl, by = c("HHID" = "HHID", "PN" = "PN")) %>%
  left_join(hrs16_func, by = c("HHID" = "HHID", "PN" = "PN"))


hrs <- hrs %>%
  mutate(cog_threshold = case_when(TF <36.0 ~ 2,
                             TF < 43.3 ~ 1,
                             !is.na(TF) ~ 0),
         iadl_threshold = case_when(cog_threshold==2 & iadl_imp>0 ~ 1,
                                    cog_threshold==2 & iadl_imp==0 ~ 0,
                                    cog_threshold==1 & iadl_imp>0 ~ 1,
                                    cog_threshold==1 & iadl_imp==0 ~ 0))

hrs <- hrs %>%
  mutate(dx = case_when(cog_threshold==2 & iadl_threshold==1 ~ 2,
                        cog_threshold==2 & iadl_threshold==0 & self_concerns==1 ~ 2,
                        cog_threshold==2 & iadl_threshold==0 & self_concerns==0 ~ 1,
                        cog_threshold==1 & iadl_threshold==1 ~ 1,
                        cog_threshold==1 & iadl_threshold==0  & self_concerns==1~ 1,
                        cog_threshold==1 & iadl_threshold==0  & self_concerns==0~ 0,
                        cog_threshold==0 ~ 0
                        ))
QSPtools::checkvar(hrs, dx, cog_threshold, iadl_threshold, self_concerns)

hrs <- hrs %>%
  labelled::set_variable_labels(dx = "Algorithm Dx") %>%
  labelled::set_value_labels(dx = c("Normal" = 0, "MCI" = 1, "Dementia" = 2))

hrs %>%
  select(dx) %>%
  labelled::to_factor() %>%
  gtsummary::tbl_summary()
