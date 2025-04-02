


hrs16_cog <- readRDS(fs::path(r_objects_folder, "025_hrs16_cog.rds"))
hrs16_cog_norm <- readRDS( fs::path(r_objects_folder, "025_hrs16_cog_norm.rds"))
hrs16_iadl <- readRDS(fs::path(r_objects_folder, "012_hrs16_iadl.rds"))
hrs16_func <- readRDS(fs::path(r_objects_folder, "010_hrs16_func.rds"))

hrshcap <- haven::read_dta(here::here("stata", "hrshcap.dta"))

hrshcap <- hrshcap %>%
  mutate(imputed_flag = case_when(vs2memsc_iflag == 0 &
                       vs2exfsc_iflag == 0 &
                       vs2lflsc_iflag == 0 &
                       vs2vissc_iflag == 0 &
                       vs2vdori1_iflag == 0 &
                       vs2memimp_iflag == 0 &
                       vs2exfimp_iflag == 0 &
                       vs2lflimp_iflag == 0 &
                       vs2memimp_eap_iflag ==0 &
                       vs2exfimp_eap_iflag ==0 &
                       vs2lflimp_eap_iflag ==0 &
                       vs2visimp_iflag ==0 &
                       vs2orimp_iflag  ==0 &
                       vs3jormsc_iflag ==0 &
                       vs3blessedsc_iflag ==0 ~ 1,
                       TRUE ~ 0))


hrs16_func <- hrs16_func  %>%
  mutate(self_concerns = case_when(PD102==3 ~ 1,
                                   PD102 %in% c(1, 2) ~ 0)) %>%
  select(HHID, PN, self_concerns)

# Using the n=3496 sample
hcap_cog <- hrs16_cog %>% filter(!is.na(normexcld))

quantile(hcap_cog$TF, probs = c(.15, .35), na.rm=TRUE)

foo <- hcap_cog %>%
  left_join(hrs16_iadl, by = c("HHID" = "HHID", "PN" = "PN")) %>%
  left_join(hrs16_func, by = c("HHID" = "HHID", "PN" = "PN")) %>%
  left_join(hrshcap, by = c("HHID" = "hhid", "PN" = "pn"))

# Filtering to the nonimputed sample (n=2993)
foo <- foo %>%
  filter(imputed_flag==1)

foo <- foo %>%
  mutate(cog_imp = case_when(TF <36.0 ~ 2,
                             TF < 43.3 ~ 1,
                             !is.na(TF) ~ 0))
gtsummary::tbl_summary(foo, include = c(cog_imp),
                       type = c(cog_imp) ~ "categorical")

foo_2 <- foo %>%
  filter(cog_imp==2)

gtsummary::tbl_summary(foo_2, include = c(iadl_imp),
                       type = c(iadl_imp) ~ "categorical")
# HCAP sample: 375/591 = 63%
# The HRS IADL cutpoints that get the closet: 1+ 217/405 = 53%

foo_1 <- foo %>%
  filter(cog_imp==1)

gtsummary::tbl_summary(foo_1, include = c(iadl_imp),
                       type = c(iadl_imp) ~ "categorical")
# HCAP sample: 556/726 = 75%
# The HRS IADL cutpoints that get the closet: 1+ 205/572 = 36%

foo <- foo %>%
  mutate(cog_threshold = case_when(TF <36.0 ~ 2,
                                   TF < 43.3 ~ 1,
                                   !is.na(TF) ~ 0),
         iadl_threshold = case_when(cog_threshold==2 & iadl_imp>0 ~ 1,
                                    cog_threshold==2 & iadl_imp==0 ~ 0,
                                    cog_threshold==1 & iadl_imp>0 ~ 1,
                                    cog_threshold==1 & iadl_imp==0 ~ 0))

foo <- foo %>%
  mutate(dx = case_when(cog_threshold==2 & iadl_threshold==1 ~ 2,
                        cog_threshold==2 & iadl_threshold==0 & self_concerns==1 ~ 2,
                        cog_threshold==2 & iadl_threshold==0 & self_concerns==0 ~ 1,
                        cog_threshold==1 & iadl_threshold==1 ~ 1,
                        cog_threshold==1 & iadl_threshold==0  & self_concerns==1~ 1,
                        cog_threshold==1 & iadl_threshold==0  & self_concerns==0~ 0,
                        cog_threshold==0 ~ 0,
                        is.na(cog_imp) & vs3jormsc >= 3.4 ~ 2,
                        is.na(cog_imp) & vs3jormsc > 3.0 &  vs3jormsc < 3.4 ~ 1,
                        is.na(cog_imp) & vs3jormsc <= 3.0  ~ 0
  ))
QSPtools::checkvar(foo, dx, cog_threshold, iadl_threshold, self_concerns)

a1 <- foo %>% filter(is.na(cog_imp)) %>% nrow(); a1
a2 <- foo %>% filter(cog_imp %in% c(0, 1)) %>% nrow(); a2
b2 <- foo %>% filter(cog_imp %in% c(2)) %>% nrow(); b2
b1 <- foo %>% filter(is.na(cog_imp) & vs3jormsc < 3.4) %>% nrow(); b1
b3 <- foo %>% filter(is.na(cog_imp) & vs3jormsc < 3) %>% nrow(); b3
c4 <- foo %>% filter(is.na(cog_imp) & vs3jormsc > 3.0 & vs3jormsc < 3.4) %>% nrow(); c4
c1 <- foo %>% filter(is.na(cog_imp) & vs3jormsc >= 3.4) %>% nrow(); c1
b4 <- foo %>% filter(cog_imp==1) %>% nrow()

c2 <- foo %>% filter(cog_imp==2 & iadl_imp>=1) %>% nrow(); c2
c3 <- foo %>% filter(cog_imp==2 & iadl_imp==0) %>% nrow(); c3
d1 <- foo %>% filter(cog_imp==2 & iadl_imp==0 & self_concerns==1) %>% nrow(); d1
d2 <- foo %>% filter(cog_imp==2 & iadl_imp==0 & self_concerns==0) %>% nrow(); d2

c5 <- foo %>% filter(cog_imp==1 & iadl_imp>=1) %>% nrow(); c5
c6 <- foo %>% filter(cog_imp==1 & iadl_imp==0) %>% nrow(); c6
d3 <- foo %>% filter(cog_imp==1 & iadl_imp==0 & self_concerns==1) %>% nrow(); d3
d4 <- foo %>% filter(cog_imp==1 & iadl_imp==0 & self_concerns==0) %>% nrow(); d4
c7 <- foo %>% filter(cog_imp==0) %>% nrow(); c7

e1 <- foo %>% filter(is.na(cog_imp) & is.na(vs3jormsc)) %>% nrow(); e1
e2 <- foo %>% filter(dx==2) %>% nrow(); e2
e3 <- foo %>% filter(dx==1) %>% nrow(); e3
e4 <- foo %>% filter(dx==0) %>% nrow(); e4

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
