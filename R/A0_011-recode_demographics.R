

tracker_demo <- readRDS(here::here("R_objects", "A0_010_tracker_demo.rds"))

# 2025-07-26
# Rich Jones Added this.I will replace missing (NA, 99) on SCHLYRS with
# a value drawn using predictive mean matching using DEGREE, GENDER, HISPANIC,
# and RACE and BIRTHYR as predictors. There are many people who have 99 for SCHLYRS but
# have information on educational attainment in the variable DEGREE.
# The new variable is called SCHLYRSimp


# Prepare data: replace 99 with NA in SCHLYRS
tracker_demo <- tracker_demo %>%
  mutate(SCHLYRS = na_if(SCHLYRS, 99),
         BIRTHYR = na_if(BIRTHYR, 0),
         )

# in file 015-Call-data-CORE-respondent.do, rnj manually recoded education for these five participants
tracker_demo <- tracker_demo %>%
  mutate(SCHLYRS = case_when(HHID==134189 & PN==020 ~ 14,
                             HHID==502334 & PN==010 ~ 12,
                             HHID==502607 & PN==010 ~ 10,
                             HHID==905576 & PN==010 ~ 16,
                             HHID==906962 & PN==010 ~ 12,
                             TRUE ~ SCHLYRS)
         )


# Set up predictors
impute_vars <- c("SCHLYRS", "DEGREE", "GENDER", "HISPANIC", "RACE", "BIRTHYR")
# Subset for imputation
imp_data <- tracker_demo %>% select(all_of(impute_vars))
# Run mice with 1 imputation using PMM
imp_result <- mice::mice(imp_data, m = 1, method = "pmm", seed = 2 , printFlag = FALSE)
# Extract completed data
completed <- mice::complete(imp_result, 1)
# Replace imputed SCHLYRS into tracker_demo
tracker_demo$SCHLYRSimp <- completed$SCHLYRS
attr(tracker_demo$SCHLYRSimp, "label") <- "Number of years in school (imputed)"

tracker_demo <- tracker_demo %>%
  mutate(
    rage = PA019,
    female = case_when(GENDER==2 ~ 1,
                       GENDER==1 ~ 0),
    black = case_when(RACE ==2 & !(HISPANIC %in% c(1, 2, 3)) ~ 1,
                      RACE ==2 & (HISPANIC %in% c(1, 2, 3)) ~ 0,
                      RACE %in% c(0, 1, 7) ~ 0),
    hisp = case_when(HISPANIC %in% c(1, 2, 3) ~ 1,
                     HISPANIC %in% c(5, 0) ~ 0)
  )




tracker_demo <- tracker_demo %>%

  labelled::set_variable_labels(female = "Female (from trk2022tr_r)") %>%
  labelled::set_value_labels(female = c("Female" = 1, "Male" = 0)) %>%

  labelled::set_variable_labels(black = "Black or African-American (not Hispanic) (from trk2022tr_r)") %>%
  labelled::set_value_labels(black = c("Black" = 1, "Non-Black" = 0)) %>%

  labelled::set_variable_labels(hisp = "Hispanic (from trk2022tr_r)") %>%
  labelled::set_value_labels(hisp = c("Hispanic" = 1, "Non-Hispanic" = 0))



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

ageRCS <- function(data, age_var, knots = c(70, 78, 86, 94)) {

  # Define the fixed knots as in the Stata code

  age <- data[[age_var]]

  k1 <- knots[1]
  k2 <- knots[2]
  k3 <- knots[3]
  k4 <- knots[4]



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

tracker_demo <- tracker_demo %>%
  ageRCS("rage")


# Age categories

tracker_demo <- tracker_demo %>%
  mutate(rage_cat = car::recode(rage, "lo:64=NA; 65:69=1; 70:74=2; 75:79=3; 80:84=4; 85:89=5; 90:hi=6"),
         rage_cat_f = factor(rage_cat, levels = 1:6, labels = c("65-69", "70-74", "75-79", "80-84", "85-89", "90 and over"), ordered = TRUE)
         )
attr(tracker_demo$rage_cat, "label") <- "R CURRENT AGE CALCULATION, grouped"
attr(tracker_demo$rage_cat_f, "label") <- "R CURRENT AGE CALCULATION, grouped"

# Sex ------------------------------------------------------------------

tracker_demo <- tracker_demo %>%
  mutate(Sex = case_when(female==1 ~ 1,
                         female==0 ~ 0),
         Sex = haven::labelled(Sex, labels = c("Male" = 0, "Female" = 1))
         )
attr(tracker_demo$Sex, "label") <- "Sex"


# Race and ethnicity -------------------------------------------------
# labelled::val_labels(tracker_demo$RACE) doesn't have any val labels
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
# 38759          5.  Non-Hispanic
#
#
# ==========================================================================================

tracker_demo <- tracker_demo %>%
  mutate(# Step 1: Assign by RACE
         RaceAndEthnicity = case_when(RACE==1 ~ 1, # White
                                      RACE==2 ~ 2, # Black or African-American
                                      RACE %in% c(0, 7) ~ 4, # All other racial groups
                                      ),
         # Step 2: Overwrite with Hispanic if applicable
         RaceAndEthnicity = case_when(HISPANIC %in% c(1, 2, 3) ~ 3, # Hispanic (any racial group)
                                      TRUE ~ RaceAndEthnicity
                                      ),
         # Step 3: Apply value labels
         RaceAndEthnicity <- labelled(RaceAndEthnicity, labels = c("White" = 1,
                                                                   "Black or African-American (Not Hispanic)" = 2,
                                                                   "Hispanic (any racial group)" = 3,
                                                                   "All other racial groups" = 4
                                                                   )
                                      )
         )

attr(tracker_demo$RaceAndEthnicity, "label") <- "Race and ethnicity"

# ---------------------------------------------------------------------------
# Educational attainment

tracker_demo <- tracker_demo %>%
  mutate(Educational_Attainment = case_when(!is.na(SCHLYRSimp) & SCHLYRSimp < 12 ~ 1,
                                            !is.na(SCHLYRSimp) & SCHLYRSimp == 12 ~ 2,
                                            !is.na(SCHLYRSimp) & SCHLYRSimp > 12 & SCHLYRSimp < 14 ~ 3,
                                            !is.na(SCHLYRSimp) & SCHLYRSimp >= 14 ~ 4,
                                            is.na(SCHLYRSimp) ~ 5),
         Educational_Attainment = haven::labelled(Educational_Attainment, labels = c("< High school" = 1,
                                                                                     "High school" = 2,
                                                                                     "Some college" = 3,
                                                                                     "Education beyond college" = 4,
                                                                                     "Unknown" = 5
                                                                                     )
                                                  )
         )

attr(tracker_demo$Educational_Attainment, "label") <- "Educational attainment"

# ---------------------------------------------------------------------------
## Covariates for adjustment models, classificaiton models
## x1-x3 = spage1, spage2, spage3
## x4 = female centered at HCAP16WGTR-weighted mean for female, using STRATUM and SECU
## x5 = black centered at HCAP16WGTR-weighted mean for black, using STRATUM and SECU
## x6 = hisp centered at HCAP16WGTR-weighted mean for hisp, using STRATUM and SECU
## x7 = SCHLYRSimp centered at 12
## all two-way interactions of x1-x7, except no interactions among x1-x3.
## e.g., name x1x4 as the interaction of x1 and x4
## label x1-x7 with the name of the source variable
## SAVE all derived means for future use in centering.
## Although means are estimated in the inHCAP==1 sample, the variables x1-x7 and their interactions


# Create analytic covariates x1–x7
tracker_demo<- tracker_demo %>%
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
attr(tracker_demo$x1, "label") <- "Source: spage1"
attr(tracker_demo$x2, "label") <- "Source: spage2"
attr(tracker_demo$x3, "label") <- "Source: spage3"
attr(tracker_demo$x4, "label") <- "Source: female"
attr(tracker_demo$x5, "label") <- "Source: black"
attr(tracker_demo$x6, "label") <- "Source: hisp"
attr(tracker_demo$x7, "label") <- "Source: SCHLYRSimp"


# Create interaction terms: all two-way combinations excluding x1:x3
covars <- paste0("x", 1:7)
for (i in seq_along(covars)) {
  for (j in seq_along(covars)) {
    if (j <= i) next                            # skip lower triangle and self
    if (i <= 3 && j <= 3) next                  # skip x1:x3 interactions
    xi <- covars[i]
    xj <- covars[j]
    new_name <- paste0(xi, xj)             # e.g., x1x4
    tracker_demo[[new_name]] <- tracker_demo[[xi]] * tracker_demo[[xj]]
  }
}

# Center covariates that need centering
# x1, x2, and x3 and x7 do not need centering
# anything interacting with x1, x2, x3, and x7 do not need centering
# x4, x5, x6, x4x5, x4x6, x5x6 all need centering

# Define your inHCAP sample
df <- tracker_demo %>%
  filter(inHCAP == 1)

# Step 1: Define the survey design
svy_design <- svydesign(
  ids = ~SECU,
  strata = ~STRATUM,
  weights = ~HCAP16WGTR,
  data = df,
  nest = TRUE
)

# Step 2: Compute weighted means for female, black, and hispanic
mean_x4   <- svymean(~x4, svy_design, na.rm = TRUE)[1]
mean_x5   <- svymean(~x5, svy_design, na.rm = TRUE)[1]
mean_x6   <- svymean(~x6, svy_design, na.rm = TRUE)[1]
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
tracker_demo <- tracker_demo %>%
  mutate(
    x4   = x4 - covariate_means$x4_mean,
    x5   = x5 - covariate_means$x5_mean,
    x6   = x6 - covariate_means$x6_mean,
    x4x5 = x4x5 - covariate_means$x4x5_mean,
    x4x6 = x4x6 - covariate_means$x4x6_mean,
    x5x6 = x5x6 - covariate_means$x5x6_mean
  )




saveRDS(tracker_demo, here::here("R_objects", "A0_011_tracker_demo.rds"))
saveRDS(covariate_means, here::here("R_objects", "A0_011_covariate_means.rds"))

