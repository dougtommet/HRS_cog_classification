


hrs16_merged <- readRDS(here::here("R_objects", "A7_005_hrs16_merged.rds"))
hrs16_22_merged <- readRDS(here::here("R_objects", "A7_005_hrs16_22_merged.rds"))

hrs16_merged <- hrs16_merged %>%
  mutate(HCAP16WGTR0 = case_when(is.na(HCAP16WGTR) ~ 0,
                                TRUE ~ HCAP16WGTR))

hrs16_nolabs <- hrs16_merged %>%
  haven::zap_labels() %>%
  mutate(mplusid = row_number()
         )

hrs16_22_nolabs <- hrs16_22_merged %>%
  haven::zap_labels() %>%
  mutate(mplusid = row_number()
  )

hrs16_nolabs <- hrs16_nolabs %>%
  mutate(vdori    = rPvdori,
         vdlfl1z  = rPvdlfl1z,
         vdlfl2   = rPvdlfl2,
         vdlfl3   = rPvdlfl3,
         vdcount  = rPvdcount,
         vdsevens = rPvdsevens,
         vdwdimm  = rPvdwdimm,
         vdwddelz = rPvdwddelz,
         vdexf7z  = rPvdexf7z   )



mplus_analysis_wlsmv = "estimator = wlsmv; COVERAGE=0; PARAMETERIZATION = THETA;
TYPE = complex;"
mplus_variable = "categorical = vdori  vdlfl2 vdlfl3 vdsevens vdcount; idvariable = mplusid;
weight = HCAP16WGTR0; stratification = STRATUM; cluster = SECU_mplus; "

mplus_output = "standardized;"
mplus_title = "vdori - Orientation to time \n
vdlfl1 - Animal naming \n
vdlfl2 - Scissors & cactus \n
vdlfl3 - President & vice-president \n
vdcount - Count backwards from 20 \n
vdsevens - Serial sevens \n
vdwdimm - Immediate word recall \n
vdwddel - Delayed word recall \n
vdexf7 - Number series \n"
mplus_model <- "f by vdori* vdlfl1z vdlfl2 vdlfl3 vdwddelz vdexf7z vdsevens vdcount;
  f @1;"
variable_list <- c("mplusid", "HCAP16WGTR0", "STRATUM", "SECU_mplus",
                   "vdori", "vdlfl1z", "vdlfl2", "vdlfl3", "vdwddelz",
                   "vdexf7z", "vdsevens", "vdcount")

# Model 1 - Removing the immediate word recall
fs::dir_create(here::here("mplus_output", "A7", "model_1"))
withr::with_dir(here::here("mplus_output", "A7", "model_1"), {

  mod1 <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = mplus_model,
    VARIABLE = mplus_variable,
    ANALYSIS = mplus_analysis_wlsmv,
    OUTPUT = mplus_output,
    SAVEDATA = "H5RESULTS = model1.h5;",
    usevariables = variable_list,
    rdata = hrs16_nolabs
  )
  MplusAutomation::mplusModeler(mod1, modelout = "model1.inp", run = 1, writeData = "always")

})



##############################################
# Model 1 - Removing the immediate word recall
# Fixing all item parameters
# Fitting the model with mlr/probit

fs::dir_create(here::here("mplus_output", "A7", "model_1a"))

mplus_analysis_mlr = "estimator = mlr; COVERAGE=0; link=PROBIT;
TYPE = complex;"

withr::with_dir(here::here("mplus_output", "A7", "model_1a"), {

  mod1a <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = mplus_model,
    VARIABLE = mplus_variable,
    ANALYSIS = mplus_analysis_mlr,
    OUTPUT = mplus_output,
    SAVEDATA = "H5RESULTS = model1a.h5;",
    usevariables = variable_list,
    rdata = hrs16_nolabs
  )
  MplusAutomation::mplusModeler(mod1a, modelout = "model1a.inp", run = 1, writeData = "always")

})


##############################
### Creating models with the item parameters fixed to model1a



model_1a_fixed_h5_path <- here::here("mplus_output", "A7", "model_1a", "model1a.h5")
# mplush5::mplus.view.results(model_1a_fixed_h5_path)
model_1a_fixed_results <- mplush5::mplus.print.model.results(model_1a_fixed_h5_path)

mplus_model_1a_fixed <- model_1a_fixed_results %>%
  # filter(!grepl("Variances", Section)) %>%
  mutate(mod = case_when(Section=="" ~ str_c(Statement, " @ ", round(Estimate, 4), "; "),
                         Section=="Intercepts" ~ str_c("[", Statement, " @ ", round(Estimate, 4), "]; "),
                         Section=="Thresholds" ~ str_c("[", Statement, " @ ", round(Estimate, 4), "]; "),
                         Section=="Residual Variances" ~ str_c(Statement, " @ ", round(Estimate, 4), "; ")
  )
  ) %>%
  filter(!is.na(mod)) %>%
  pull(mod) %>%
  str_c(collapse = " \n ")
mplus_model_1a_fixed <- str_c(mplus_model_1a_fixed, " \n [F@0]; \n F@1;")

fs::dir_create(here::here("mplus_output", "A7", "model_1a_fixed"))

model_1a_fixed_fscores <- withr::with_dir(here::here("mplus_output", "A7", "model_1a_fixed"), {

  mod_final <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = mplus_model_1a_fixed,
    VARIABLE = mplus_variable,
    ANALYSIS = mplus_analysis_mlr,
    OUTPUT = mplus_output,
    SAVEDATA = "H5RESULTS = model_1a_fixed.h5; save = fscores; file = model_1a_fixed.dat;",
    usevariables = variable_list,
    rdata = hrs16_nolabs
  )
  MplusAutomation::mplusModeler(mod_final, modelout = "model_1a_fixed.inp", run = 1, writeData = "always")

  model_1a_fixed <- MplusAutomation::readModels("model_1a_fixed.out")
  model_1a_fixed[["savedata"]] %>%
    tibble() %>%
    select(MPLUSID, F, F_SE)

})


hrs16_temp <- hrs16_nolabs %>%
  left_join(model_1a_fixed_fscores, by = c("mplusid" = "MPLUSID"))

hrs16_fscores <- hrs16_temp %>%
  # mutate(F = case_when(HCAP16WGTR0==0 ~ NA_real_,
  #                      HCAP16WGTR0 >0 ~ F),
  #        F_SE = case_when(HCAP16WGTR0==0 ~ NA_real_,
  #                         HCAP16WGTR0 >0 ~ F_SE)) %>%
  select(id, F, F_SE) %>%
  rename(PF = F,
         PF_SE = F_SE) %>%
  labelled::set_variable_labels(PF = "Factor score (2016)")

hrs16_merged <- hrs16_merged %>%
  left_join(hrs16_fscores, by = c("id" = "id"))

################################################
fs::dir_create(here::here("mplus_output", "A7", "model_1a_fixed_2016_2022"))

foo <- hrs16_22_nolabs %>%
  select(mplusid, SECU_mplus, STRATUM, starts_with("inHRS"), ends_with("WGTR"),
         ends_with("vdori"),
         ends_with("vdlfl1z"),
         ends_with("vdlfl2"),
         ends_with("vdlfl3"),
         ends_with("vdcount"),
         ends_with("vdsevens"),
         ends_with("vdwdimm"),
         ends_with("vdwddelz"),
         ends_with("vdexf7z")

         )

foo_long <- foo %>%
  # pivot cognitive variables
  pivot_longer(
    cols = matches("^r(P|Q|R|S)"),
    names_to = c("wave", ".value"),
    names_pattern = "^r(P|Q|R|S)(.+)$"
  ) %>%
  # pivot inHRS columns and join
  left_join(
    foo %>%
      select(mplusid, inHRS_16, inHRS_18, inHRS_20, inHRS_22) %>%
      pivot_longer(
        cols = -mplusid,
        names_to = "wave",
        names_pattern = "inHRS_(\\d+)",
        values_to = "inHRS"
      ) %>%
      mutate(wave = case_when(
        wave == "16" ~ "P",
        wave == "18" ~ "Q",
        wave == "20" ~ "R",
        wave == "22" ~ "S"
      )),
    by = c("mplusid", "wave")
  ) %>%
  # pivot weights and join
  left_join(
    foo %>%
      select(mplusid, PWGTR, QWGTR, RWGTR, SWGTR) %>%
      pivot_longer(
        cols = -mplusid,
        names_to = "wave",
        names_pattern = "(P|Q|R|S)WGTR",
        values_to = "weight"
      ),
    by = c("mplusid", "wave")
  ) %>%
  select(mplusid, SECU_mplus, STRATUM, wave, inHRS, weight,
         vdori, vdlfl1z, vdlfl2, vdlfl3, vdcount, vdsevens,
         vdwdimm, vdwddelz, vdexf7z) %>%
  mutate(mplusid2 = row_number())



mplus_variable_2016_2022 <- gsub("HCAP16WGTR0", "weight", mplus_variable, fixed = TRUE)
mplus_variable_2016_2022 <- gsub("mplusid", "mplusid2", mplus_variable_2016_2022, fixed = TRUE)
variable_list_2016_2022 <- c(variable_list[!variable_list %in% c("HCAP16WGTR0", "mplusid")], "weight", "mplusid2")


model_1a_fixed_fscores <- withr::with_dir(here::here("mplus_output", "A7", "model_1a_fixed_2016_2022"), {

mod_final <- MplusAutomation::mplusObject(
  TITLE = mplus_title,
  MODEL = mplus_model_1a_fixed,
  VARIABLE = mplus_variable_2016_2022,
  ANALYSIS = mplus_analysis_mlr,
  OUTPUT = mplus_output,
  SAVEDATA = "H5RESULTS = model_1a_fixed.h5; save = fscores; file = model_1a_fixed.dat;",
  usevariables = variable_list_2016_2022,
  rdata = foo_long

)
MplusAutomation::mplusModeler(mod_final, modelout = "model_1a_fixed.inp", run = 1, writeData = "always")

model_1a_fixed <- MplusAutomation::readModels("model_1a_fixed.out")
model_1a_fixed_fscores <- model_1a_fixed[["savedata"]] %>%
  tibble() %>%
  select(MPLUSID2, F, F_SE)
})

hrs16_22_long <- foo_long %>%
  left_join(model_1a_fixed_fscores, by = c("mplusid2" = "MPLUSID2"))  %>%
  labelled::set_variable_labels(F = "Factor score") %>%
  left_join(hrs16_22_nolabs %>%
              select(id, HHID, PN, mplusid),
            by = c("mplusid" = "mplusid")) %>%
  select(id, HHID, PN, everything())



# hrs16_22_wide_temp <- hrs16_22_long %>%
#   select(mplusid, wave, F, F_SE) %>%
#   pivot_wider(names_from = wave,
#               values_from = c(F, F_SE),
#               names_glue = "{wave}{.value}")  %>%
#   labelled::set_variable_labels(PF = "Factor score (2016)") %>%
#   labelled::set_variable_labels(QF = "Factor score (2018)") %>%
#   labelled::set_variable_labels(RF = "Factor score (2020)") %>%
#   labelled::set_variable_labels(SF = "Factor score (2022)")
#
# hrs16_22_wide_temp <- hrs16_22_nolabs %>%
#   left_join(hrs16_22_wide_temp, by = "mplusid") %>%
#   select(id, PF, QF, RF, SF, PF_SE, QF_SE, RF_SE, SF_SE)

# hrs16_22_merged <- hrs16_22_merged %>%
#   left_join(hrs16_22_wide_temp, by = "id")




################################################
# Summary values for reports (Appendix 1)

read_out_count <- function(path, label) {
  x <- readLines(path, warn = FALSE)
  hit <- grep(label, x, value = TRUE)[1]
  as.integer(stringr::str_extract(hit, "[0-9]+\\s*$"))
}

cfa_items <- c("vdori", "vdlfl1z", "vdlfl2", "vdlfl3",
               "vdwddelz", "vdexf7z", "vdsevens", "vdcount")

cfa_out_wlsmv <- here::here("mplus_output", "A7", "model_1", "model1.out")
cfa_out_mlr   <- here::here("mplus_output", "A7", "model_1a", "model1a.out")
cfa_wlsmv <- MplusAutomation::readModels(cfa_out_wlsmv, quiet = TRUE)
cfa_mlr   <- MplusAutomation::readModels(cfa_out_mlr, quiet = TRUE)

std_loadings <- function(m) {
  m$parameters$stdyx.standardized %>%
    filter(paramHeader == "F.BY") %>%
    transmute(item = str_to_lower(param), est, se)
}

A7_050_cfa_summary <- list(
  items = cfa_items,
  n_obs = cfa_wlsmv$summaries$Observations,
  n_strata   = read_out_count(cfa_out_wlsmv, "Number of strata"),
  n_clusters = read_out_count(cfa_out_wlsmv, "Number of clusters"),
  n_positive_weight = sum(hrs16_nolabs$HCAP16WGTR0 > 0),
  n_positive_weight_any_item = hrs16_nolabs %>%
    filter(HCAP16WGTR0 > 0) %>%
    filter(if_any(all_of(cfa_items), ~ !is.na(.x))) %>%
    nrow(),
  fit_wlsmv = cfa_wlsmv$summaries %>%
    select(any_of(c("ChiSqM_Value", "ChiSqM_DF", "ChiSqM_PValue",
                    "RMSEA_Estimate", "RMSEA_90CI_LB", "RMSEA_90CI_UB",
                    "CFI", "TLI", "SRMR"))),
  # Proportion with each item present, among HRS-HCAP participants (weight > 0)
  # with at least 1 item response: the cases that inform the estimates.
  coverage_hcap = hrs16_nolabs %>%
    filter(HCAP16WGTR0 > 0) %>%
    filter(if_any(all_of(cfa_items), ~ !is.na(.x))) %>%
    summarise(across(all_of(cfa_items), ~ mean(!is.na(.x)))) %>%
    unlist(),
  # Weighted (HCAP16WGTR) share in the highest observed category, categorical items.
  top_share = sapply(c("vdori", "vdlfl2", "vdlfl3", "vdsevens", "vdcount"), function(v) {
    d <- hrs16_nolabs %>% filter(HCAP16WGTR0 > 0, !is.na(.data[[v]]))
    sum(d$HCAP16WGTR0[d[[v]] == max(d[[v]])]) / sum(d$HCAP16WGTR0)
  }),
  loadings_wlsmv = std_loadings(cfa_wlsmv),
  loadings_mlr   = std_loadings(cfa_mlr),
  item_coverage_by_wave = foo_long %>%
    filter(inHRS == 1) %>%
    group_by(wave) %>%
    summarise(n = n(),
              across(all_of(cfa_items), ~ sum(!is.na(.x))),
              .groups = "drop"),
  # Person-waves Mplus scored (at least 1 item response; all ages, 2016-2022).
  n_person_waves_scored = read_out_count(
    here::here("mplus_output", "A7", "model_1a_fixed_2016_2022", "model_1a_fixed.out"),
    "Number of observations"
  )
)



################################################
# Appendix 1: item coverage by wave and EAP shrinkage (added 2026-10-18)
# Population: Core respondents interviewed in the wave, aged 65+, with a
# nonzero wave weight (xWGTR). Proxy interviews are included and have no tests.

wave_info <- tibble::tribble(
  ~L,  ~year, ~age_var, ~in_var,
  "P", 2016L, "page",   "inHRS_16",
  "Q", 2018L, "qage",   "inHRS_18",
  "R", 2020L, "rage",   "inHRS_20",
  "S", 2022L, "sage",   "inHRS_22"
)
coverage_items <- c(cfa_items, "vdwdimmz")   # immediate recall: not in the model

wave_frame <- function(L, age_var, in_var) {
  d <- hrs16_22_nolabs %>%
    filter(.data[[in_var]] == 1) %>%
    transmute(mplusid,
              age = .data[[age_var]],
              w   = .data[[paste0(L, "WGTR")]],
              across(any_of(paste0("r", L, coverage_items)))) %>%
    rename_with(~ sub("^r[PQRS]", "", .x), starts_with("r")) %>%
    filter(!is.na(w), w > 0, !is.na(age), age >= 65)
  for (it in coverage_items) if (!it %in% names(d)) d[[it]] <- NA_real_
  d
}

item_coverage_65 <- purrr::pmap_dfr(wave_info, function(L, year, age_var, in_var) {
  d <- wave_frame(L, age_var, in_var)
  present <- lapply(coverage_items, function(it) !is.na(d[[it]]))
  names(present) <- coverage_items
  present[["any_item"]] <- Reduce(`|`, present)
  tibble::tibble(
    year = year,
    item = names(present),
    n_respondents = nrow(d),
    n_present = vapply(present, sum, numeric(1)),
    p_present_weighted = vapply(present, function(ok) sum(d$w[ok]) / sum(d$w), numeric(1))
  )
})

# EAP factor scores (fixed 2016 model) by wave, same population.
age_long <- purrr::pmap_dfr(wave_info, function(L, year, age_var, in_var) {
  hrs16_22_nolabs %>% transmute(mplusid, wave = L, age = .data[[age_var]])
})
eap_65 <- hrs16_22_long %>%
  left_join(age_long, by = c("mplusid", "wave")) %>%
  filter(inHRS == 1, !is.na(weight), weight > 0, !is.na(age), age >= 65, !is.na(F)) %>%
  select(wave, F, F_SE, weight)

# Shrinkage comparison: waves with number series administered (2016, 2020)
# versus 2018 (not administered). 2022 is left out (number series missing for
# most face-to-face interviews).
shrink_groups <- c(P = "2016 and 2020", R = "2016 and 2020", Q = "2018")
eap_shrink <- eap_65 %>%
  filter(wave %in% names(shrink_groups)) %>%
  mutate(group = factor(shrink_groups[wave], levels = c("2016 and 2020", "2018")))

eap_shrink_summary <- eap_shrink %>%
  group_by(group) %>%
  summarise(
    n = n(),
    min = min(F), q1 = quantile(F, 0.25), median = median(F),
    q3 = quantile(F, 0.75), max = max(F),
    sd = sd(F),
    mean_psd = mean(F_SE),
    mean_psd2 = mean(F_SE^2),
    .groups = "drop"
  ) %>%
  mutate(var_eap = sd^2,
         total = var_eap + mean_psd2)   # law of total variance: about 1 under the model

A7_050_cfa_summary$item_coverage_65   <- item_coverage_65
A7_050_cfa_summary$eap_shrink         <- eap_shrink
A7_050_cfa_summary$eap_shrink_summary <- eap_shrink_summary
saveRDS(A7_050_cfa_summary, here::here("R_objects", "A7_050_cfa_summary.rds"))

saveRDS(hrs16_merged, here::here("R_objects", "A7_050_hrs16_merged.rds"))
# saveRDS(hrs16_22_merged, here::here("R_objects", "A7_050_hrs16_22_merged.rds"))
saveRDS(hrs16_22_long, here::here("R_objects", "A7_050_hrs16_22_long.rds"))







