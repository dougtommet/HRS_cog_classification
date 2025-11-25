# In this file I make some variables for tables and for analyses.

source(here::here("R","PMM_027_custom_functions.R"))
PMM_45 <- readRDS(here::here("R_objects", "PMM_041.RDS"))


# Age splines
# I use the same rule for RCS definition for age splines as was used in the HCAP analyses.
# It is important that this RCS definition:
#   1. Be included in a function for application to future data sets
#   2. Not rely upon sample (current data set) distributions of age
#
# Here are notes from ../HCAP23/POSTED/ANALYSIS/Integrated_Analysis/041-Normalization-Standardization.do
# 3. Regression adjustment
#    Regress each Blom-transformed factor score (separately) on age, sex,
#    race, Hispanic ethnicity, and educational attainment.
#
#    Age is modeled as a continuous predictor using restricted cubic
#    splines with knots at 70, 78, 86, 94 (on a range of 65-103). These
#    knots were chosen ad hoc using an empirical process, and fall at
#    the 25th, 60th, 88th, and 99th percentiles of \verb+hcapage+.
#
#    The somewhat unusual choice of knot locations is driven by the
#    cross-sectional relationship between age and cognitive test score.
#    The shape is distinctly hockey-stick-shaped relationship where a
#    nearly linear performance-age relationship is seen through most of
#    the age range (older people performing worse) but then the direction
#    shifts and older people perform better.
#
#    This effect is likely caused by the retention of only the most
#    cognitively-intact persons among the oldest-old following our
#    exclusions from the norming sample. The knot choice is meant to get
#    more parameters estimated in the region where the age-performance
#    relationship is more dynamic.
#
#    Sex is modeled as male and female using a dummy variable
#
#    Race and ethnicity are coarsely modeled with two dummy variables,
#    one indicating Black or African-American, the other Hispanic ethnicty.
#
#    Education is included as a continuous predcitor (0-17).
#
#           Note: we compared different ways for controlling for education
#              - A continuous variable (0-17)
#              - A categorical predictor identifying the following
#                groups defined in terms of years of completed schooling:
#                    0, 1-8, 9-11, 12, 13-15, 16, 17 and higher
#              - A restricted cubic spline with 4 knots placed at default
#                locations
#              - A set of models including two linear splines with knots
#                placed from 4 to 15 years
#
#              To evaluate these alternative parameterizations, we
#              regressed the estimated GCP (EAP), GCP (PV), MEM (EAP),
#              LFL (EAP), vdori1, vdvis1, h1rmseotal on each of the above
#              representations of education. For all except vdori1 and
#              h1rmseotal the model with the lowest BIC was the continuous
#              linear function of number of years of education.
#              Orientation favored two linear splines with a knot at 13
#              years of education, and the MMSE (h1rmsetotal) preferred the
#              restricted cubic splines.
#
#              Based on the predominance of evidence, we decided to keep
#              education as a continuous predictor.
#
#    Main effects and two-way interactions} are included. The only
#    two-way interaction that is not included is black*hisp, because
#    in sample there are no persons both Black and Hispanic.

ageRCS <- function(data, age_var) {
  age_var_quo <- rlang::enquo(age_var)
  age_name <- rlang::as_label(age_var_quo)
  age <- data[[age_name]]

  # Define the fixed knots as in the Stata code
  k1 <- 70
  k2 <- 78
  k3 <- 86
  k4 <- 94

  # Define the truncated power basis function
  tp <- function(x, knot) {
    pmax((x - knot)^3, 0)
  }

  # Compute each spline component
  spage1 <- age

  denom <- (k4 - k1)^2
  part1 <- tp(age, k3)
  part2 <- tp(age, k4)
  spage2 <- (tp(age, k1) -
    ((k4 - k3)^-1) * (part1 * (k4 - k1) - part2 * (k3 - k1))) / denom

  part1_b <- tp(age, k3)
  part2_b <- tp(age, k4)
  spage3 <- (tp(age, k2) -
    ((k4 - k3)^-1) * (part1_b * (k4 - k2) - part2_b * (k3 - k2))) / denom

  # Handle missing values (preserve NAs from original age variable)
  spage2[is.na(age)] <- NA
  spage3[is.na(age)] <- NA

  # Center spage1 at age 70. This is new (not done in HRS/HCAP analyses)
  spage1 <- spage1 - 70  # center at age 70

  # Add to data frame
  data$spage1 <- spage1
  data$spage2 <- spage2
  data$spage3 <- spage3

  return(data)
}

PMM_45 <- PMM_45 |> ageRCS(rage)

# PMM_45 |> select(spage1, spage2, spage3) |> skimr::skim()
#
# library(tidyr)
# library(ggplot2)
#
# # Assuming you already have spage1, spage2, spage3, and rage in PMM_45
#
# # Step 1: reshape the spline components to long format
# plot_data <- PMM_45 |>
#   select(rage, spage1, spage2, spage3) |>
#   pivot_longer(cols = starts_with("spage"),
#                names_to = "spline_component",
#                values_to = "value") |>
#   drop_na() |>
#   arrange(spline_component, rage)  # <--- ensure correct draw order
#
# # Step 2: plot
# ggplot(plot_data, aes(x = rage, y = value, color = spline_component)) +
#   geom_line() +
#   labs(
#     title = "Restricted Cubic Spline Basis Functions",
#     x = "Age (rage)",
#     y = "Component Value",
#     color = "Component"
#   ) +
#   theme_minimal()
#
#
# # ----------------------------------------------------------------------
# # ----------------------------------------------------------------------
# # Variables for tables
# #
# # ----------------------------------------------------------------------
# Age categories
library(haven)    # for labelled class
library(dplyr)    # for data wrangling

# Define breaks and labels
breaks <- c(-Inf, 69, 74, 79, 84, 89, Inf)
labels <- c(
  "65-69",
  "70-74",
  "75-79",
  "80-84",
  "85-89",
  "90 and over"
)
values <- 1:6  # value codes to go with labels

# Create factor with codes, then convert to labelled
PMM_45$rage_cat <- cut(PMM_45$rage, breaks = breaks, labels = values, right = TRUE, include.lowest = TRUE)
PMM_45$rage_cat <- as.numeric(as.character(PMM_45$rage_cat))  # convert factor to numeric code

# Set to NA if rage < 65
PMM_45$rage_cat[PMM_45$rage < 65] <- NA

# Apply value labels and variable label
PMM_45$rage_cat <- labelled(
  PMM_45$rage_cat,
  labels = setNames(values, labels)
)
attr(PMM_45$rage_cat, "label") <- "Age group, y"

# Start Check rage_cat
library(dplyr)
library(haven)
library(labelled)

# Extract value labels from rage_cat
rage_labels <- val_labels(PMM_45$rage_cat)

# Convert to data frame for joining
label_df <- tibble(
  code = as.numeric(rage_labels),
  label = names(rage_labels)
)

# # Summarize
# summary_table <- PMM_45 |>
#   filter(!is.na(rage_cat)) |>
#   group_by(rage_cat) |>
#   summarise(
#     n = n(),
#     prop = n() / nrow(PMM_45),
#     min_rage = min(rage, na.rm = TRUE),
#     max_rage = max(rage, na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   left_join(label_df, by = c("rage_cat" = "code")) |>
#   select(rage_cat, label, n, prop, min_rage, max_rage)
#
# library(flextable)
#
# flextable::flextable(summary_table) |>
#   flextable::colformat_num(j = "prop", digits = 3) |>
#   flextable::set_header_labels(
#     rage_cat = "Category Code",
#     label = "Age Group",
#     n = "N",
#     prop = "Proportion",
#     min_rage = "Min Age",
#     max_rage = "Max Age"
#   )
# End check rage_cat
# ----------------------------------------------------------------------

# Sex ------------------------------------------------------------------
PMM_45$Sex <- labelled(
  ifelse(PMM_45$female == 1, 1,
         ifelse(PMM_45$female == 0, 0, NA)),  # preserve NAs
  labels = c("Male" = 0, "Female" = 1)
)
attr(PMM_45$Sex, "label") <- "Sex"
#
# labelled::look_for(PMM_45, "Sex")
# table(PMM_45$Sex, useNA = "ifany")
# val_labels(PMM_45$Sex)
# ---------------------------------------------------------------------

# Race and ethnicity -------------------------------------------------
# val_labels(PMM_45$RACE) dosen't have any val labels
# ==========================================================================================
#
#
#   RACE                     RACE/ETHNICITY
# Section: TR    Level: Respondent      Type: Numeric    Width: 1   Decimals: 0
#
# .................................................................................
# 2157           0.  Not obtained
# 31781          1.  White/Caucasian
# 9008           2.  Black or African American
# 3904           7.  Other
#
#
# ==========================================================================================
#
#
#   HISPANIC                 HISPANICITY TYPE
# Section: TR    Level: Respondent      Type: Numeric    Width: 1   Decimals: 0
#
# .................................................................................
# 2113           0.  Not obtained
# 3362           1.  Hispanic, Mexican
# 2565           2.  Hispanic, Other
# 51             3.  Hispanic, type unknown
# 38759           5.  Non-Hispanic
#
#
# ==========================================================================================

# Initialize RaceAndEthnicity from RACE
PMM_45$RaceAndEthnicity <- NA_integer_

# Step 1: Assign by RACE
PMM_45$RaceAndEthnicity[PMM_45$RACE == 1] <- 1  # White
PMM_45$RaceAndEthnicity[PMM_45$RACE == 2] <- 2  # Black or African-American
PMM_45$RaceAndEthnicity[PMM_45$RACE %in% c(0, 7)] <- 4  # All other racial groups

# Step 2: Overwrite with Hispanic if applicable
PMM_45$RaceAndEthnicity[PMM_45$HISPANIC %in% c(1, 2, 3)] <- 3  # Hispanic (any racial group)

# Step 3: Apply value labels and variable label
PMM_45$RaceAndEthnicity <- labelled(
  PMM_45$RaceAndEthnicity,
  labels = c(
    "White" = 1,
    "Black or African-American (Not Hispanic)" = 2,
    "Hispanic (any racial group)" = 3,
    "All other racial groups" = 4
  )
)

attr(PMM_45$RaceAndEthnicity, "label") <- "Race and ethnicity"

# ---------------------------------------------------------------------------
# Educational attainment

# Initialize to NA
PMM_45$Educational_Attainment <- NA_integer_

# Assign based on SCHLYRS
PMM_45$Educational_Attainment[!is.na(PMM_45$SCHLYRSimp) & PMM_45$SCHLYRSimp < 12] <- 1
PMM_45$Educational_Attainment[!is.na(PMM_45$SCHLYRSimp) & PMM_45$SCHLYRSimp == 12] <- 2
PMM_45$Educational_Attainment[!is.na(PMM_45$SCHLYRSimp) & PMM_45$SCHLYRSimp > 12 & PMM_45$SCHLYRSimp < 14] <- 3
PMM_45$Educational_Attainment[!is.na(PMM_45$SCHLYRSimp) & PMM_45$SCHLYRSimp >= 14] <- 4
PMM_45$Educational_Attainment[is.na(PMM_45$SCHLYRSimp)] <- 5

# Apply value labels and variable label
PMM_45$Educational_Attainment <- labelled(
  PMM_45$Educational_Attainment,
  labels = c(
    "< High school" = 1,
    "High school" = 2,
    "Some college" = 3,
    "Education beyond college" = 4,
    "Unknown" = 5
  )
)

attr(PMM_45$Educational_Attainment, "label") <- "Educational attainment"

## Covariates for adjustment models, classificaiton models
## x1-x3 = spage1, spage2, spage3
## x4 = female centered at HCAP16WGTR-weighted mean for female, using STRATUM and SECU
## x5 = black centered at HCAP16WGTR-weighted mean for black, using STRATUM and SECU
## x6 = hisp centered at HCAP16WGTR-weighted mean for hisp, using STRATUM and SECU
## x7 = SCHLYRSimp centered at 12
## all two-way interactions of x1-x7, except no interactions among x1-x3.
## e.g., name x1x4 as the interaction of x1 and x4
## label x1-x7 with the name of the source variable
## SAVE all derived means for future use in centring.
## Althoug means are estimated in the inHCAP==1 sample, the variables x1-x7 and their interactions
## PMM_45 |> dplyr::filter(inHCAP==1) |> dplyr::select(spage1, spage2, spage3, female, black, hisp, SCHLYRSimp, STRATUM, SECU) |> skimr::skim()




# Create analytic covariates x1–x7
PMM_45 <- PMM_45 %>%
  mutate(
    x1 = spage1, # already centered at 70
    x2 = spage2, # already 0 at 70
    x3 = spage3, # already 0 at 70
    x4 = female,
    x5 = black,
    x6 = hisp,
    x7 = SCHLYRSimp-12
  )
# Apply variable labels (assuming haven::labelled)

for (i in 1:7) {
  attr(PMM_45[[paste0("x", i)]], "label") <- paste("Source:", c("spage1", "spage2", "spage3", "female", "black", "hisp", "SCHLYRSimp")[i])
}
# Create interaction terms: all two-way combinations excluding x1:x3
covars <- paste0("x", 1:7)
for (i in seq_along(covars)) {
  for (j in seq_along(covars)) {
    if (j <= i) next                            # skip lower triangle and self
    if (i <= 3 && j <= 3) next                  # skip x1:x3 interactions
    xi <- covars[i]
    xj <- covars[j]
    new_name <- paste0(xi, xj)             # e.g., x1x4
    PMM_45[[new_name]] <- PMM_45[[xi]] * PMM_45[[xj]]
  }
}
# cat(paste(names(PMM_45), collapse = ", "))


# Center covariates that need centering
# x1, x2, and x3 and x7 do not need centering
# anything interacting with x1, x2, x3, and x7 do not need centering
# x4, x5, x6, x4x5, x4x6, x5x6 all need centering

# Define your inHCAP sample
df <- PMM_45 %>% filter(inHCAP == 1)

# Step 1: Define the survey design
svy_design <- svydesign(
  ids = ~SECU,
  strata = ~STRATUM,
  weights = ~HCAP16WGTR,
  data = df,
  nest = TRUE
)

# Step 2: Compute weighted means for female, black, and hispanic
mean_x4 <- svymean(~x4, svy_design, na.rm = TRUE)[1]
mean_x5 <- svymean(~x5, svy_design, na.rm = TRUE)[1]
mean_x6 <- svymean(~x6, svy_design, na.rm = TRUE)[1]
mean_x4x5 <- svymean(~x4x5, svy_design, na.rm = TRUE)[1]
mean_x4x6 <- svymean(~x4x6, svy_design, na.rm = TRUE)[1]
mean_x5x6 <- svymean(~x5x6, svy_design, na.rm = TRUE)[1]

# Store the means for future use
covariate_means <- list(
  x4_mean = as.numeric(mean_x4),
  x5_mean  = as.numeric(mean_x5),
  x6_mean   = as.numeric(mean_x6),
  x4x5_mean = as.numeric(mean_x4x5),
  x4x6_mean = as.numeric(mean_x4x6),
  x5x6_mean = as.numeric(mean_x5x6)
)

# Center select covariates
PMM_45 <- PMM_45 %>%
  mutate(
    x4 = x4 - covariate_means$x4_mean,
    x5 = x5 - covariate_means$x5_mean,
    x6 = x6 - covariate_means$x6_mean,
    x4x5 = x4x5 - covariate_means$x4x5_mean,
    x4x6 = x4x6 - covariate_means$x4x6_mean,
    x5x6 = x5x6 - covariate_means$x5x6_mean
  )
vars_to_unlabel <- c(
  "x1x4", "x1x5", "x1x6", "x1x7",
  "x2x4", "x2x5", "x2x6", "x2x7",
  "x3x4", "x3x5", "x3x6", "x3x7",
  "x4x5", "x4x6", "x4x7",
  "x5x6", "x5x7", "x6x7"
)

for (var in vars_to_unlabel) {
  attr(PMM_45[[var]], "label") <- NULL
}

saveRDS(PMM_45, here::here("R_objects", "PMM_045.RDS"))

