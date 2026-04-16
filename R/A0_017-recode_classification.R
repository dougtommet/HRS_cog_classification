
langa_weir <- readRDS(here::here("R_objects", "A0_005_langa_weir.rds"))
hudomiet   <- readRDS(here::here("R_objects", "A0_005_hudomiet.rds"))
# hurd      <-  readRDS(here::here("R_objects", "A0_005_hurd.rds"))


langa_weir <- langa_weir %>%
  rename(HHID = hhid, PN = pn) %>%
  select(HHID, PN, cogfunction2016)

### Hurd dementia probabilities are only available 199, 2001, 2003, 2005, 2007
# hurd <- hurd %>%
#   rename(HHID = hhid, PN = pn) %>%
#   select(HHID, PN, prob_dementia, prediction_year)

### Luckily, Hudomiet is available "From waves 5 (year 2000) to 13 (year 2016)."
hudomiet <- hudomiet %>%
  filter(wave == 13) %>%
  select(hhidpn, PrDem, PrCIND, PrNorm, Cog, CogSd)

hudomiet <- hudomiet %>%
  mutate(hhidpn_str = sprintf("%009.0f", hhidpn),
         hhidpn_rev = stringi::stri_reverse(hhidpn_str),
         NP = substr(hhidpn_rev, 1, 3),
         PN = stringi::stri_reverse(NP),
         DIHH = substr(hhidpn_rev, 4, 9),
         HHID = stringi::stri_reverse(DIHH)) %>%
  select(-hhidpn, -hhidpn_str, -hhidpn_rev, -NP, -DIHH)

# create 3-level classification
hudomiet <- hudomiet %>%
  mutate(Hudomiet_classification = case_when(PrNorm >= PrCIND & PrNorm >= PrDem ~ 1,
                                             PrCIND >= PrDem & PrCIND > PrNorm  ~ 2,
                                             PrDem > PrCIND & PrDem > PrNorm    ~ 3,
                                             TRUE ~ NA_integer_
                                             ),
    Hudomiet_classification = haven::labelled(Hudomiet_classification,
                                              labels = c("Normal" = 1,
                                                         "CIND" = 2,
                                                         "Dementia" = 3
                                                         )
                                              )
    )

# Assign variable label
attr(hudomiet$Hudomiet_classification, "label") <- "Hudomiet classification by highest class proportion"

saveRDS(langa_weir,        here::here("R_objects", "A0_017_langa_weir.rds"))
# saveRDS(hurd,              here::here("R_objects", "A0_017_hurd.rds"))
saveRDS(hudomiet,          here::here("R_objects", "A0_017_hudomiet.rds"))
