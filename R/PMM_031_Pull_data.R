# Select the variables of interest
# Cognitive Data
cogdata <- readRDS(here::here("R_objects", "025_hrs16_cog.rds"))  |>
  dplyr::select(
    HHID, PN, normexcld,
    rage, PA019, female, black, hisp, SCHLYRS, SCHLYRSimp, DEGREE, HISPANIC, RACE,
    vdori, vdlfl1z, vdlfl2, vdlfl3,
    vdwdimmz, vdwddelz, vdexf7z, vdsevens, vdcount
  )
# Self-Concerns and Jorm
scjormdata <- readRDS(here::here("R_objects", "013_hrs16_func.rds")) |>
  dplyr::select(
    HHID, PN,
    PD102, jorm
  )
## Functional items
fxndata <- readRDS(here::here("R_objects","012_hrs16_iadl.rds")) |>
  dplyr::select(
    HHID, PN,
    rPG014, rPG021, rPG023, rPG030,
    rPG040, rPG041, rPG044, rPG047,rPG050, rPG059,
    iadl_imp,
    PG013,
    PG014,   PG021,  PG023,  PG030,
    PG040,  PG041,  PG044,  PG047,  PG050,  PG059,
  )
## HCAP classification
hcapdata <- readRDS(here::here("R_objects","014_hrshcap.rds")) |>
  dplyr::select(hhid, pn, vs1hcapdx, vs1hcapdxeap)
## Tracker information
trkdata <- readRDS(here::here("R_objects","005_tracker.rds")) |>
  dplyr::select(HHID, PN,
                SECU, STRATUM, PWGTR,
                PAGE, PINSAMP, PIWWAVE, PIWYEAR, PMARST, PNURSHM, PPROXY)

# HCAP Weigt
hcapwgtdata <- readRDS(here::here("R_objects","005_hc16hp_r.rds")) |>
  dplyr::select(hhid, pn, HCAP16WGTR)

# Need to get HRS/HCAP validation data (N=50)
hcapN50data <- haven::read_dta(here::here("Stata","20240228-040.dta")) |>
  dplyr::select(hhid, pn, samplingP, consensuspaneldx)


# haven::zap_label() |> skimr::skim()


# Merge

# Harmonize variable names in hcapdata
hcapdata   <- dplyr::rename(hcapdata,     HHID = hhid, PN = pn)
hcapwgtdata <- dplyr::rename(hcapwgtdata, HHID = hhid, PN = pn)
hcapN50data <- dplyr::rename(hcapN50data, HHID = hhid, PN = pn)

### Langa-Weir, Hurd, 005_hudomiet.rds
lw2016 <- readRDS(here::here("R_objects", "005_langa_weir.rds")) |> dplyr::rename(HHID = hhid, PN = pn) |> dplyr::select(HHID, PN, cogfunction2016)
### Hurd dementia probabilities are only available 199, 2001, 2003, 2005, 2007
### hurd <- readRDS(here::here("R_objects", "005_hurd.rds")) |> dplyr::rename(HHID = hhid, PN = pn) #|> dplyr::select(HHID, PN, prob_dementia)
### |> dplyr::rename(HHID = hhid, PN = pn) |> dplyr::select(HHID, PN, cogfunction2016)
### Luckily, Hudomiet is available "From waves 5 (year 2000) to 13 (year 2016)."
Hudomiet <- readRDS(here::here("R_objects", "005_hudomiet.rds")) |> dplyr::filter(wave == 13) |> dplyr::select(hhidpn, PrDem, PrCIND, PrNorm, Cog, CogSd)
Hudomiet <- Hudomiet |>
  dplyr::mutate(hhidpn_str = sprintf("%009.0f", hhidpn)) |>
  dplyr::mutate(hhidpn_rev = stringi::stri_reverse(hhidpn_str)) |>
  dplyr::mutate(NP = substr(hhidpn_rev, 1, 3)) |>
  dplyr::mutate(PN = stringi::stri_reverse(NP)) |>
  dplyr::mutate(DIHH = substr(hhidpn_rev, 4, 9)) |>
  dplyr::mutate(HHID = stringi::stri_reverse(DIHH)) |>
  dplyr::select(-hhidpn, -hhidpn_str, -hhidpn_rev, -NP, -DIHH)
# create 3-level classification
Hudomiet <- Hudomiet |>
  dplyr::mutate(
    Hudomiet_classification = dplyr::case_when(
      PrNorm >= PrCIND & PrNorm >= PrDem ~ 1,
      PrCIND >= PrDem & PrCIND > PrNorm  ~ 2,
      PrDem > PrCIND & PrDem > PrNorm    ~ 3,
      TRUE ~ NA_integer_
    ),
    Hudomiet_classification = haven::labelled(
      Hudomiet_classification,
      labels = c(
        "Normal" = 1,
        "CIND" = 2,
        "Dementia" = 3
      )
    )
  )
# Assign variable label
attr(Hudomiet$Hudomiet_classification, "label") <- "Hudomiet classification by highest class proportion"
#cat(paste(names(Hudomiet), collapse = ", "))



#Hudomiet |> skimr::skim()
#fxndata |> dplyr::select(HHID, PN) |> head()
#Hudomiet |> dplyr::select(HHID, PN) |> head()
# Merge using cogdata as base
PMM_31 <- cogdata |>
  dplyr::left_join(scjormdata, by = c("HHID", "PN")) |>
  dplyr::left_join(fxndata,    by = c("HHID", "PN")) |>
  dplyr::left_join(hcapdata,   by = c("HHID", "PN")) |>
  dplyr::left_join(hcapwgtdata,   by = c("HHID", "PN")) |>
  dplyr::left_join(hcapN50data,   by = c("HHID", "PN")) |>
  dplyr::left_join(trkdata,   by = c("HHID", "PN")) |>
  dplyr::left_join(lw2016,    by = c("HHID", "PN")) |>
  dplyr::left_join(Hudomiet,    by = c("HHID", "PN"))

# cat(paste(names(PMM_31), collapse = ", "))

# Reorder variables
PMM_31 <- PMM_31 |>
  dplyr::select(
    HHID, PN,
    SECU, STRATUM, PWGTR, HCAP16WGTR, samplingP,
    PINSAMP, PIWWAVE, PIWYEAR, PMARST, PNURSHM, PPROXY,
    rage, PA019, PAGE, female, black, hisp, SCHLYRS, HISPANIC, RACE,
    vdori, vdlfl1z, vdlfl2, vdlfl3, vdwdimmz, vdwddelz, vdexf7z, vdsevens, vdcount,
    rPG014, rPG021, rPG023, rPG030, rPG040, rPG041, rPG044, rPG047, rPG050, rPG059, iadl_imp,
    PG013,
    PG014,   PG021,  PG023,  PG030,  PG040,  PG041,  PG044,  PG047,  PG050,  PG059,
    PD102,
    jorm,
    normexcld, vs1hcapdx, vs1hcapdxeap, consensuspaneldx,
    cogfunction2016, # Langa-Weir
    PrDem, PrCIND, PrNorm, Cog, CogSd, Hudomiet_classification, # you guessed it: Hudomiet
    dplyr::everything()     # All remaining variables (not yet listed)
  )

#PMM_31 |> haven::zap_label() |> skimr::skim()

# Save the merged data
saveRDS(PMM_31, here::here("R_objects", "PMM_031.RDS"))
