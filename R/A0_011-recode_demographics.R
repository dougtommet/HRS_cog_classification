

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
    page = PA019,
    qage = QA019,
    rage = RA019,
    sage = SA019,
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
  ageRCS("page") %>%
  rename(spage1_16 = spage1,
         spage2_16 = spage2,
         spage3_16 = spage3)

tracker_demo <- tracker_demo %>%
  ageRCS("qage") %>%
  rename(spage1_18 = spage1,
         spage2_18 = spage2,
         spage3_18 = spage3)
tracker_demo <- tracker_demo %>%
  ageRCS("rage") %>%
  rename(spage1_20 = spage1,
         spage2_20 = spage2,
         spage3_20 = spage3)
tracker_demo <- tracker_demo %>%
  ageRCS("sage") %>%
  rename(spage1_22 = spage1,
         spage2_22 = spage2,
         spage3_22 = spage3)


# Age categories

tracker_demo <- tracker_demo %>%
  mutate(page_cat = car::recode(page, "lo:64=NA; 65:69=1; 70:74=2; 75:79=3; 80:84=4; 85:89=5; 90:hi=6"),
         page_cat_f = factor(page_cat, levels = 1:6, labels = c("65-69", "70-74", "75-79", "80-84", "85-89", "90 and over"), ordered = TRUE),
         qage_cat = car::recode(qage, "lo:64=NA; 65:69=1; 70:74=2; 75:79=3; 80:84=4; 85:89=5; 90:hi=6"),
         qage_cat_f = factor(qage_cat, levels = 1:6, labels = c("65-69", "70-74", "75-79", "80-84", "85-89", "90 and over"), ordered = TRUE),
         rage_cat = car::recode(rage, "lo:64=NA; 65:69=1; 70:74=2; 75:79=3; 80:84=4; 85:89=5; 90:hi=6"),
         rage_cat_f = factor(rage_cat, levels = 1:6, labels = c("65-69", "70-74", "75-79", "80-84", "85-89", "90 and over"), ordered = TRUE),
         sage_cat = car::recode(sage, "lo:64=NA; 65:69=1; 70:74=2; 75:79=3; 80:84=4; 85:89=5; 90:hi=6"),
         sage_cat_f = factor(sage_cat, levels = 1:6, labels = c("65-69", "70-74", "75-79", "80-84", "85-89", "90 and over"), ordered = TRUE)
         )
attr(tracker_demo$page_cat, "label")   <- "CURRENT AGE CALCULATION (2016), grouped"
attr(tracker_demo$page_cat_f, "label") <- "CURRENT AGE CALCULATION (2016), grouped"
attr(tracker_demo$qage_cat, "label")   <- "CURRENT AGE CALCULATION (2018), grouped"
attr(tracker_demo$qage_cat_f, "label") <- "CURRENT AGE CALCULATION (2018), grouped"
attr(tracker_demo$rage_cat, "label")   <- "CURRENT AGE CALCULATION (2020), grouped"
attr(tracker_demo$rage_cat_f, "label") <- "CURRENT AGE CALCULATION (2020), grouped"
attr(tracker_demo$sage_cat, "label")   <- "CURRENT AGE CALCULATION (2022), grouped"
attr(tracker_demo$sage_cat_f, "label") <- "CURRENT AGE CALCULATION (2022), grouped"

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
         RaceAndEthnicity =  labelled(RaceAndEthnicity, labels = c("White" = 1,
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
## x1-x3 = spage1_16, spage2_16, spage3_16
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
    xP1 = case_when(inHRS_16==1 ~ spage1_16), # already centered at 70
    xP2 = case_when(inHRS_16==1 ~ spage2_16), # already 0 at 70
    xP3 = case_when(inHRS_16==1 ~ spage3_16), # already 0 at 70
    xP4 = case_when(inHRS_16==1 ~ female),
    xP5 = case_when(inHRS_16==1 ~ black),
    xP6 = case_when(inHRS_16==1 ~ hisp),
    xP7 = case_when(inHRS_16==1 ~ SCHLYRSimp-12)
  )
# Apply variable labels (assuming haven::labelled)
attr(tracker_demo$xP1, "label") <- "Source: spage1_16"
attr(tracker_demo$xP2, "label") <- "Source: spage2_16"
attr(tracker_demo$xP3, "label") <- "Source: spage3_16"
attr(tracker_demo$xP4, "label") <- "Source: female"
attr(tracker_demo$xP5, "label") <- "Source: black"
attr(tracker_demo$xP6, "label") <- "Source: hisp"
attr(tracker_demo$xP7, "label") <- "Source: SCHLYRSimp"

tracker_demo<- tracker_demo %>%
  mutate(
    xQ1 = case_when(inHRS_18==1 ~ spage1_18), # already centered at 70
    xQ2 = case_when(inHRS_18==1 ~ spage2_18), # already 0 at 70
    xQ3 = case_when(inHRS_18==1 ~ spage3_18), # already 0 at 70
    xQ4 = case_when(inHRS_18==1 ~ female),
    xQ5 = case_when(inHRS_18==1 ~ black),
    xQ6 = case_when(inHRS_18==1 ~ hisp),
    xQ7 = case_when(inHRS_18==1 ~ SCHLYRSimp-12)
  )
attr(tracker_demo$xQ1, "label") <- "Source: spage1_18"
attr(tracker_demo$xQ2, "label") <- "Source: spage2_18"
attr(tracker_demo$xQ3, "label") <- "Source: spage3_18"
attr(tracker_demo$xQ4, "label") <- "Source: female"
attr(tracker_demo$xQ5, "label") <- "Source: black"
attr(tracker_demo$xQ6, "label") <- "Source: hisp"
attr(tracker_demo$xQ7, "label") <- "Source: SCHLYRSimp"

tracker_demo<- tracker_demo %>%
  mutate(
    xR1 = case_when(inHRS_20==1 ~ spage1_20), # already centered at 70
    xR2 = case_when(inHRS_20==1 ~spage2_20), # already 0 at 70
    xR3 = case_when(inHRS_20==1 ~spage3_20), # already 0 at 70
    xR4 = case_when(inHRS_20==1 ~female),
    xR5 = case_when(inHRS_20==1 ~black),
    xR6 = case_when(inHRS_20==1 ~hisp),
    xR7 = case_when(inHRS_20==1 ~SCHLYRSimp-12)
  )
attr(tracker_demo$xR1, "label") <- "Source: spage1_20"
attr(tracker_demo$xR2, "label") <- "Source: spage2_20"
attr(tracker_demo$xR3, "label") <- "Source: spage3_20"
attr(tracker_demo$xR4, "label") <- "Source: female"
attr(tracker_demo$xR5, "label") <- "Source: black"
attr(tracker_demo$xR6, "label") <- "Source: hisp"
attr(tracker_demo$xR7, "label") <- "Source: SCHLYRSimp"

tracker_demo<- tracker_demo %>%
  mutate(
    xS1 = case_when(inHRS_22==1 ~ spage1_22), # already centered at 70
    xS2 = case_when(inHRS_22==1 ~ spage2_22), # already 0 at 70
    xS3 = case_when(inHRS_22==1 ~ spage3_22), # already 0 at 70
    xS4 = case_when(inHRS_22==1 ~ female),
    xS5 = case_when(inHRS_22==1 ~ black),
    xS6 = case_when(inHRS_22==1 ~ hisp),
    xS7 = case_when(inHRS_22==1 ~ SCHLYRSimp-12)
  )
attr(tracker_demo$xS1, "label") <- "Source: spage1_22"
attr(tracker_demo$xS2, "label") <- "Source: spage2_22"
attr(tracker_demo$xS3, "label") <- "Source: spage3_22"
attr(tracker_demo$xS4, "label") <- "Source: female"
attr(tracker_demo$xS5, "label") <- "Source: black"
attr(tracker_demo$xS6, "label") <- "Source: hisp"
attr(tracker_demo$xS7, "label") <- "Source: SCHLYRSimp"


# Create interaction terms: all two-way combinations excluding x1:x3
covars <- paste0("xP", 1:7)
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

tracker_demo <- tracker_demo %>%
  select(-xP5xP6)

attr(tracker_demo$xP1xP4, "label") <- "Interaction: spage1_16 X female"
attr(tracker_demo$xP1xP5, "label") <- "Interaction: spage1_16 X black"
attr(tracker_demo$xP1xP6, "label") <- "Interaction: spage1_16 X hisp"
attr(tracker_demo$xP1xP7, "label") <- "Interaction: spage1_16 X SCHLYRS"
attr(tracker_demo$xP2xP4, "label") <- "Interaction: spage2_16 X female"
attr(tracker_demo$xP2xP5, "label") <- "Interaction: spage2_16 X black"
attr(tracker_demo$xP2xP6, "label") <- "Interaction: spage2_16 X hisp"
attr(tracker_demo$xP2xP7, "label") <- "Interaction: spage2_16 X SCHLYRS"
attr(tracker_demo$xP3xP4, "label") <- "Interaction: spage3_16 X female"
attr(tracker_demo$xP3xP5, "label") <- "Interaction: spage3_16 X blalck"
attr(tracker_demo$xP3xP6, "label") <- "Interaction: spage3_16 X hisp"
attr(tracker_demo$xP3xP7, "label") <- "Interaction: spage3_16 X SCHLYRS"
attr(tracker_demo$xP4xP5, "label") <- "Interaction: female X black"
attr(tracker_demo$xP4xP6, "label") <- "Interaction: female X hisp"
attr(tracker_demo$xP4xP7, "label") <- "Interaction: female X SCHLYRS"
attr(tracker_demo$xP5xP7, "label") <- "Interaction: black X SCHLYYRS"
attr(tracker_demo$xP6xP7, "label") <- "Interaction: hisp X SCHLYRS"

covars <- paste0("xQ", 1:7)
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

tracker_demo <- tracker_demo %>%
  select(-xQ5xQ6)

attr(tracker_demo$xQ1xQ4, "label") <- "Interaction: spage1_18 X female"
attr(tracker_demo$xQ1xQ5, "label") <- "Interaction: spage1_18 X black"
attr(tracker_demo$xQ1xQ6, "label") <- "Interaction: spage1_18 X hisp"
attr(tracker_demo$xQ1xQ7, "label") <- "Interaction: spage1_18 X SCHLYRS"
attr(tracker_demo$xQ2xQ4, "label") <- "Interaction: spage2_18 X female"
attr(tracker_demo$xQ2xQ5, "label") <- "Interaction: spage2_18 X black"
attr(tracker_demo$xQ2xQ6, "label") <- "Interaction: spage2_18 X hisp"
attr(tracker_demo$xQ2xQ7, "label") <- "Interaction: spage2_18 X SCHLYRS"
attr(tracker_demo$xQ3xQ4, "label") <- "Interaction: spage3_18 X female"
attr(tracker_demo$xQ3xQ5, "label") <- "Interaction: spage3_18 X blalck"
attr(tracker_demo$xQ3xQ6, "label") <- "Interaction: spage3_18 X hisp"
attr(tracker_demo$xQ3xQ7, "label") <- "Interaction: spage3_18 X SCHLYRS"
attr(tracker_demo$xQ4xQ5, "label") <- "Interaction: female X black"
attr(tracker_demo$xQ4xQ6, "label") <- "Interaction: female X hisp"
attr(tracker_demo$xQ4xQ7, "label") <- "Interaction: female X SCHLYRS"
attr(tracker_demo$xQ5xQ7, "label") <- "Interaction: black X SCHLYYRS"
attr(tracker_demo$xQ6xQ7, "label") <- "Interaction: hisp X SCHLYRS"

covars <- paste0("xR", 1:7)
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

tracker_demo <- tracker_demo %>%
  select(-xR5xR6)

attr(tracker_demo$xR1xR4, "label") <- "Interaction: spage1_20 X female"
attr(tracker_demo$xR1xR5, "label") <- "Interaction: spage1_20 X black"
attr(tracker_demo$xR1xR6, "label") <- "Interaction: spage1_20 X hisp"
attr(tracker_demo$xR1xR7, "label") <- "Interaction: spage1_20 X SCHLYRS"
attr(tracker_demo$xR2xR4, "label") <- "Interaction: spage2_20 X female"
attr(tracker_demo$xR2xR5, "label") <- "Interaction: spage2_20 X black"
attr(tracker_demo$xR2xR6, "label") <- "Interaction: spage2_20 X hisp"
attr(tracker_demo$xR2xR7, "label") <- "Interaction: spage2_20 X SCHLYRS"
attr(tracker_demo$xR3xR4, "label") <- "Interaction: spage3_20 X female"
attr(tracker_demo$xR3xR5, "label") <- "Interaction: spage3_20 X blalck"
attr(tracker_demo$xR3xR6, "label") <- "Interaction: spage3_20 X hisp"
attr(tracker_demo$xR3xR7, "label") <- "Interaction: spage3_20 X SCHLYRS"
attr(tracker_demo$xR4xR5, "label") <- "Interaction: female X black"
attr(tracker_demo$xR4xR6, "label") <- "Interaction: female X hisp"
attr(tracker_demo$xR4xR7, "label") <- "Interaction: female X SCHLYRS"
attr(tracker_demo$xR5xR7, "label") <- "Interaction: black X SCHLYYRS"
attr(tracker_demo$xR6xR7, "label") <- "Interaction: hisp X SCHLYRS"

covars <- paste0("xS", 1:7)
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

tracker_demo <- tracker_demo %>%
  select(-xS5xS6)

attr(tracker_demo$xS1xS4, "label") <- "Interaction: spage1_22 X female"
attr(tracker_demo$xS1xS5, "label") <- "Interaction: spage1_22 X black"
attr(tracker_demo$xS1xS6, "label") <- "Interaction: spage1_22 X hisp"
attr(tracker_demo$xS1xS7, "label") <- "Interaction: spage1_22 X SCHLYRS"
attr(tracker_demo$xS2xS4, "label") <- "Interaction: spage2_22 X female"
attr(tracker_demo$xS2xS5, "label") <- "Interaction: spage2_22 X black"
attr(tracker_demo$xS2xS6, "label") <- "Interaction: spage2_22 X hisp"
attr(tracker_demo$xS2xS7, "label") <- "Interaction: spage2_22 X SCHLYRS"
attr(tracker_demo$xS3xS4, "label") <- "Interaction: spage3_22 X female"
attr(tracker_demo$xS3xS5, "label") <- "Interaction: spage3_22 X blalck"
attr(tracker_demo$xS3xS6, "label") <- "Interaction: spage3_22 X hisp"
attr(tracker_demo$xS3xS7, "label") <- "Interaction: spage3_22 X SCHLYRS"
attr(tracker_demo$xS4xS5, "label") <- "Interaction: female X black"
attr(tracker_demo$xS4xS6, "label") <- "Interaction: female X hisp"
attr(tracker_demo$xS4xS7, "label") <- "Interaction: female X SCHLYRS"
attr(tracker_demo$xS5xS7, "label") <- "Interaction: black X SCHLYYRS"
attr(tracker_demo$xS6xS7, "label") <- "Interaction: hisp X SCHLYRS"


# Center covariates that need centering
# xP1, xP2, and xP3 and xP7 do not need centering
# anything interacting with xP1, xP2, xP3, and xP7 do not need centering
# xP4, xP5, xP6, xP4xP5, xP4xP6, xP5xP6 all need centering

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
mean_xP4   <- svymean(~xP4, svy_design, na.rm = TRUE)[1]
mean_xP5   <- svymean(~xP5, svy_design, na.rm = TRUE)[1]
mean_xP6   <- svymean(~xP6, svy_design, na.rm = TRUE)[1]
mean_xP4xP5 <- svymean(~xP4xP5, svy_design, na.rm = TRUE)[1]
mean_xP4xP6 <- svymean(~xP4xP6, svy_design, na.rm = TRUE)[1]
# mean_xP5xP6 <- svymean(~xP5xP6, svy_design, na.rm = TRUE)[1]

# Store the means for future use
covariate_means <- list(
  xP4_mean    = as.numeric(mean_xP4),
  xP5_mean    = as.numeric(mean_xP5),
  xP6_mean    = as.numeric(mean_xP6),
  xP4xP5_mean = as.numeric(mean_xP4xP5),
  xP4xP6_mean = as.numeric(mean_xP4xP6)
  # xP5xP6_mean = as.numeric(mean_xP5xP6)
)

# Center select covariates
tracker_demo <- tracker_demo %>%
  mutate(
    xP4    = xP4    - covariate_means$xP4_mean,
    xP5    = xP5    - covariate_means$xP5_mean,
    xP6    = xP6    - covariate_means$xP6_mean,
    xP4xP5 = xP4xP5 - covariate_means$xP4xP5_mean,
    xP4xP6 = xP4xP6 - covariate_means$xP4xP6_mean,
    # xP5xP6 = xP5xP6 - covariate_means$xP5xP6_mean

    xQ4    = xQ4    - covariate_means$xP4_mean,
    xQ5    = xQ5    - covariate_means$xP5_mean,
    xQ6    = xQ6    - covariate_means$xP6_mean,
    xQ4xQ5 = xQ4xQ5 - covariate_means$xP4xP5_mean,
    xQ4xQ6 = xQ4xQ6 - covariate_means$xP4xP6_mean,
    # xQ5xQ6 = xQ5xQ6 - covariate_means$xP5xP6_mean

    xR4    = xR4    - covariate_means$xP4_mean,
    xR5    = xR5    - covariate_means$xP5_mean,
    xR6    = xR6    - covariate_means$xP6_mean,
    xR4xR5 = xR4xR5 - covariate_means$xP4xP5_mean,
    xR4xR6 = xR4xR6 - covariate_means$xP4xP6_mean,
    # xR5xR6 = xR5xR6 - covariate_means$xP5xP6_mean

    xS4    = xS4    - covariate_means$xP4_mean,
    xS5    = xS5    - covariate_means$xP5_mean,
    xS6    = xS6    - covariate_means$xP6_mean,
    xS4xS5 = xS4xS5 - covariate_means$xP4xP5_mean,
    xS4xS6 = xS4xS6 - covariate_means$xP4xP6_mean,
    # xS5xS6 = xS5xS6 - covariate_means$xP5xP6_mean
  )




saveRDS(tracker_demo, here::here("R_objects", "A0_011_tracker_demo.rds"))
saveRDS(covariate_means, here::here("R_objects", "A0_011_covariate_means.rds"))

