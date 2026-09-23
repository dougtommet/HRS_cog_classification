

# pmm_102 <- MplusAutomation::readModels(here::here("mplus_output", "pmm_102", "pmm_hcap_102.out"))
# write_lca_model(pmm_102, mplus_mod_norms)

write_lca_model <- function(mod, mplus_mod_norms) {
# Reading in the parameter estimates
  par_est <- mod[["parameters"]][["unstandardized"]] %>%
    as_tibble()

  par_est <- par_est %>%
    mutate(paramHeader = stringr::str_to_lower(paramHeader),
           paramHeader = stringr::str_replace(paramHeader, "\\.", " "),
           param = stringr::str_to_lower(param))


  # Filtering out the robust norms parameters.  Will use the model statement that's
  # already created
  par_est <- par_est %>%
    filter(!grepl("x[1-7]", param)) %>%
    select(paramHeader, param, est, LatentClass)

  # Making a wide version of estimates by latent class
  par_est1 <- par_est %>%
    filter(LatentClass==1) %>%
    rename(est1 = est) %>%
    select(-LatentClass)

  par_est2 <- par_est %>%
    filter(LatentClass==2) %>%
    rename(est2 = est) %>%
    select(-LatentClass)

  par_est3 <- par_est %>%
    filter(LatentClass==3) %>%
    rename(est3 = est) %>%
    select(-LatentClass)

  par_est_wide <- par_est1 %>%
    left_join(par_est2, by = c("paramHeader", "param")) %>%
    left_join(par_est3, by = c("paramHeader", "param"))

  # Many of the parameters are the same across class
  # Creating an indicator if they are equal
  par_est_wide <- par_est_wide %>%
    mutate(all_equal_12 = est1==est2,
           all_equal_23 = est2==est3,
           overall_class = all_equal_12 == TRUE & all_equal_23==TRUE) %>%
    select(-all_equal_12, -all_equal_23)

  # Writing a model statement for the gcp portion of the model, overall and by class
  mod_gcp_overall <- par_est_wide %>%
    filter(overall_class==TRUE) %>%
    filter(!paramHeader %in% c("means", "intercepts", "thresholds", "variances", "residual variances")) %>%
    mutate(mod = str_c(paramHeader, " ", param, " @ ", est1, "; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  mod_gcp_1 <- par_est_wide %>%
    filter(overall_class==FALSE) %>%
    filter(!paramHeader %in% c("means", "intercepts", "thresholds", "variances", "residual variances")) %>%
    mutate(mod = str_c(paramHeader, " ", param, " @ ", est1, "; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  mod_gcp_2 <- par_est_wide %>%
    filter(overall_class==FALSE) %>%
    filter(!paramHeader %in% c("means", "intercepts", "thresholds", "variances", "residual variances")) %>%
    mutate(mod = str_c(paramHeader, " ", param, " @ ", est2, "; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  mod_gcp_3 <- par_est_wide %>%
    filter(overall_class==FALSE) %>%
    filter(!paramHeader %in% c("means", "intercepts", "thresholds", "variances", "residual variances")) %>%
    mutate(mod = str_c(paramHeader, " ", param, " @ ", est3, "; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  # Writing a model statement for the means, overall and by class
  mod_means_overall <- par_est_wide %>%
    filter(overall_class==TRUE) %>%
    filter(paramHeader %in% c("means", "intercepts", "thresholds")) %>%
    mutate(mod = str_c("[ ", param, " @ ", est1, "] ; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  mod_means_1 <- par_est_wide %>%
    filter(overall_class==FALSE) %>%
    filter(paramHeader %in% c("means", "intercepts", "thresholds")) %>%
    mutate(mod = str_c("[ ", param, " @ ", est1, "] ; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  mod_means_2 <- par_est_wide %>%
    filter(overall_class==FALSE) %>%
    filter(paramHeader %in% c("means", "intercepts", "thresholds")) %>%
    mutate(mod = str_c("[ ", param, " @ ", est2, "] ; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  mod_means_3 <- par_est_wide %>%
    filter(overall_class==FALSE) %>%
    filter(paramHeader %in% c("means", "intercepts", "thresholds")) %>%
    mutate(mod = str_c("[ ", param, " @ ", est3, "] ; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  # Writing a model statement for the variances, overall and by class
  mod_variances_overall <- par_est_wide %>%
    filter(overall_class==TRUE) %>%
    filter(paramHeader %in% c("variances", "residual variances")) %>%
    mutate(mod = str_c(param, " @ ", est1, " ; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  mod_variances_1 <- par_est_wide %>%
    filter(overall_class==FALSE) %>%
    filter(paramHeader %in% c("variances", "residual variances")) %>%
    mutate(mod = str_c(param, " @ ", est1, " ; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  mod_variances_2 <- par_est_wide %>%
    filter(overall_class==FALSE) %>%
    filter(paramHeader %in% c("variances", "residual variances")) %>%
    mutate(mod = str_c(param, " @ ", est2, " ; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")

  mod_variances_3 <- par_est_wide %>%
    filter(overall_class==FALSE) %>%
    filter(paramHeader %in% c("variances", "residual variances")) %>%
    mutate(mod = str_c(param, " @ ", est3, " ; ")) %>%
    pull(mod) %>%
    str_c(collapse = " \n ")


  mplus_mod_fixed <- str_c("%OVERALL%", " \n ",
                           mplus_mod_norms, " \n ",
                           mod_gcp_overall, " \n ",
                           mod_means_overall, " \n ",
                           mod_variances_overall, " \n ",
                           "%c#1% ", " \n ",
                           mod_gcp_1, " \n ",
                           mod_means_1, " \n ",
                           mod_variances_1, " \n ",
                           "%c#2% ", " \n ",
                           mod_gcp_2, " \n ",
                           mod_means_2, " \n ",
                           mod_variances_2, " \n ",
                           "%c#3% ", " \n ",
                           mod_gcp_3, " \n ",
                           mod_means_3, " \n ",
                           mod_variances_3, " \n "
  )

  mplus_mod_fixed
}


# write_class_logits(pmm_103)
# Returns an Mplus statement that fixes the class logits of the fixed-parameter
# model at the known-class estimates, e.g. "[ c#1 @ 1.819 c#2 @ 0.768 ];".
# Without this, Mplus re-estimates the class proportions by EM in the scoring
# sample, and the posterior class probabilities no longer use the calibration
# priors (plug-in Bayes rule; McLachlan, 1992).
write_class_logits <- function(mod, known_class = "cg", fixed_class = "c") {
  logits <- mod[["parameters"]][["unstandardized"]] %>%
    as_tibble() %>%
    filter(tolower(paramHeader) == "means",
           grepl(str_c("^", known_class, "#"), param, ignore.case = TRUE)) %>%
    mutate(param = str_replace(str_to_lower(param),
                               str_c("^", str_to_lower(known_class)),
                               fixed_class))

  if (nrow(logits) == 0 || anyNA(logits$est)) {
    stop("No class logits found for known class '", known_class, "'.")
  }

  str_c("[ ", str_c(logits$param, " @ ", logits$est, collapse = " "), " ];")
}


# weighted_class_logits(inhcap, "vs1hcapdxeap", "HCAP16WGTR")
# Returns an Mplus statement that fixes the class logits at the survey-weighted
# class proportions of the known-class variable, with the last class as the
# reference, e.g. "[ c#1 @ 2.019 c#2 @ 0.842 ];".
# Used because Mplus KNOWNCLASS estimates of the class logits reproduce the
# unweighted class proportions even when WEIGHT is specified.
weighted_class_logits <- function(data, class_var, weight_var, fixed_class = "c") {
  props <- data %>%
    filter(!is.na(.data[[class_var]]), !is.na(.data[[weight_var]])) %>%
    group_by(class = .data[[class_var]]) %>%
    summarise(w = sum(.data[[weight_var]]), .groups = "drop") %>%
    arrange(class) %>%
    mutate(p = w / sum(w))

  if (nrow(props) < 2 || any(props$p <= 0)) {
    stop("Weighted class proportions must be positive for every class.")
  }

  k <- nrow(props)
  logits <- log(props$p[-k] / props$p[k])
  str_c("[ ", str_c(fixed_class, "#", seq_len(k - 1), " @ ",
                    format(logits, digits = 8), collapse = " "), " ];")
}
