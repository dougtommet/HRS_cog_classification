source(here::here("R","PMM_027_custom_functions.R"))

PMM_41 <- readRDS(here::here("R_objects", "PMM_033.RDS"))

library(haven)  # for variable labels

# Begin new code for ADL impairment indicators
# Variables to process
source_vars <- c("PG014", "PG021", "PG023", "PG030")
new_vars <- paste0("n", source_vars)
# Value map
recode_map <- c("1" = 1, "5" = 0, "6" = 1, "9" = 1, "7" = NA, "8" = NA)
# Loop through variables
for (i in seq_along(source_vars)) {
  src <- source_vars[i]
  new <- new_vars[i]
  # Step 1: recode values
  PMM_41[[new]] <- dplyr::recode(as.character(PMM_41[[src]]), !!!recode_map, .default = NA_real_)
  PMM_41[[new]] <- as.numeric(PMM_41[[new]])  # make sure it's numeric
  # Step 2: copy and modify label
  original_label <- attr(PMM_41[[src]], "label")
  if (!is.null(original_label)) {
    attr(PMM_41[[new]], "label") <- paste0("recoded: ", original_label)
  }
  # Step 3: fill in missing values with 0 if PG013 == 0
  PMM_41[[new]][is.na(PMM_41[[new]]) & PMM_41$PG013 == 0] <- 0
}
# More skip recodes
PMM_41$nPG021[PMM_41$PG014 == 5 & is.na(PMM_41$nPG021)] <- 0
PMM_41$nPG023[PMM_41$PG014 == 5 & is.na(PMM_41$nPG023)] <- 0
PMM_41$nPG030[PMM_41$PG014 == 5 & is.na(PMM_41$nPG030)] <- 0
# END ADL impairment indicators
# For checking
PMM_41$PG013isNot0 <- dplyr::case_when(
  is.na(PMM_41$PG013)        ~ NA_real_,
  PMM_41$PG013 == 0          ~ 0,
  PMM_41$PG013 > 0           ~ 1
)
# end for checking
# print("The r variables are old recodes, the n variables are new recodes that assign 0s for skip missing")
# PMM_41 |>  dplyr::filter(age65up == 1) |> dplyr::select(rPG014, rPG021, rPG023, rPG030, new_vars) |> skimr::skim()

# Now update the IADL coding, and rename the variables so they don't get confused
# Vector of stubs
stubs <- c("PG040", "PG041", "PG044", "PG047", "PG050", "PG059")

# Loop through each stub
for (stub in stubs) {
  r_var <- paste0("r", stub)
  base_var <- stub
  n_var <- paste0("n", stub)

  # Set r<stub> to 1 where <stub> is 6 or 9
  PMM_41[[r_var]][PMM_41[[base_var]] %in% c(6, 9)] <- 1

  # Rename r<stub> to n<stub>
  names(PMM_41)[names(PMM_41) == r_var] <- n_var
}
#PMM_41 |> dplyr::filter(age65up==1) |> QSPtools::checkvar(PG040,nPG040)


# drop out old and original functional items were're not using
# cat(paste(names(PMM_41), collapse = ", "))
vars_to_drop <- c("PG014", "PG021", "PG023", "PG030", "rPG014", "rPG021", "rPG023", "rPG030", "PG013", "PG013isNot0", "iadl_imp", "PG040", "PG041", "PG044", "PG047", "PG050", "PG059")
PMM_41 <- dplyr::select(PMM_41, -all_of(vars_to_drop))
# reorder variables
PMM_41 <- PMM_41 |>
  dplyr::select(
    HHID, PN, SECU, STRATUM, PWGTR, HCAP16WGTR, samplingP, PINSAMP, PIWWAVE, PIWYEAR, PMARST, PNURSHM, PPROXY,
    rage, PA019, PAGE, female, black, hisp, SCHLYRS, HISPANIC, RACE,
    vdori, vdlfl1z, vdlfl2, vdlfl3, vdwdimmz, vdwddelz, vdexf7z, vdsevens, vdcount,
    nPG014, nPG021, nPG023, nPG030,
    nPG040, nPG041, nPG044, nPG047, nPG050, nPG059,
    PD102, jorm,
    normexcld, vs1hcapdx, vs1hcapdxeap, consensuspaneldx, nonzeroweight, inHCAP, age65up,
    dplyr::everything()     # All remaining variables (not yet listed)
  )

saveRDS(PMM_41, here::here("R_objects", "PMM_041.RDS"))


