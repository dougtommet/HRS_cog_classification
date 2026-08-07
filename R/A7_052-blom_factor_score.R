

hrs16_merged <- readRDS(here::here("R_objects", "A7_050_hrs16_merged.rds"))
# hrs16_22_merged <- readRDS(here::here("R_objects", "A7_050_hrs16_22_merged.rds"))
hrs16_22_long <- readRDS(here::here("R_objects", "A7_050_hrs16_22_long.rds"))

hrs16_cog_norm <- hrs16_merged %>%
  filter(normexcld==0) %>%
  mutate(HCAP16WGTR0 = case_when(is.na(HCAP16WGTR) ~ 0,
                                 TRUE ~ HCAP16WGTR))
# Norming the factor score
# 1. Create restricted cubic splines of the factor score for all the data frames
# 2. In the normative HCAP sample
#   - Blom the factor score
#   - Fit a regression model for Blom factor score on the cubic splines
# 3. Apply the regression model to other data frames to get a predicted blom score




##################
## Function to get the splines

factor_splines <- function(df, q) {
  df <- df %>%
    mutate(k1 = q[[1]],
           k2 = q[[2]],
           k3 = q[[3]],
           k4 = q[[4]],
           spF1 = PF,
           spF2 = case_when(!is.na(PF) ~
                              (pmax((PF - k1)^3, 0) -
                                 (k4 - k3)^-1 *
                                 (pmax((PF - k3)^3, 0) * (k4 - k1) -
                                    pmax((PF - k4)^3, 0) * (k3 - k1))) /
                              (k4 - k1)^2),
           spF3 = case_when(!is.na(PF) ~
                              (pmax((PF - k2)^3, 0) -
                                 (k4 - k3)^-1 *
                                 (pmax((PF - k3)^3, 0) * (k4 - k2) -
                                    pmax((PF - k4)^3, 0) * (k3 - k2))) /
                              (k4 - k1)^2)
    ) %>%
    select(-k1, -k2, -k3, -k4)

  df <- df %>%
    labelled::set_variable_labels(spF1 = "F spline 1") %>%
    labelled::set_variable_labels(spF2 = "F spline 2") %>%
    labelled::set_variable_labels(spF3 = "F spline 3")

  df
}
q <- quantile(hrs16_cog_norm$PF, c(.05, .35, .65, .95), na.rm=TRUE)

# Create the cubic splines for F in the norming sample
hrs16_cog_norm <- factor_splines(hrs16_cog_norm, q)
# Create the cubic splines for F in the full (2016 HCAP) sample
hrs16_merged <- factor_splines(hrs16_merged, q)
# Create the cubic splines for F in the 2016-2022 waves
hrs16_22_long <- hrs16_22_long %>%
  mutate(PF = F) %>%
  factor_splines(q) %>%
  select(-PF)


#### Blom transform the factor score
# Blom transform the factor score in the norming sample
hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(f_blom = rcompanion::blom(PF)
  )

# Estimate the model to predict f_blom from F (using cubic splines)
transform_F_to_blom <- glm(f_blom ~ spF1 + spF2 + spF3, data = hrs16_cog_norm)

# Get the predicted f_blom values and merge into the norming sample
hrs16_cog_norm <- broom::augment(transform_F_to_blom, newdata = hrs16_cog_norm) %>%
  rename(Pblom = .fitted)

hrs16_cog_norm <- hrs16_cog_norm %>%
  labelled::set_variable_labels(Pblom = "Predicted Blom score (model from HCAP normal sample)")


# Get the predicted f_blom values and merge into the full sample
hrs16_merged <- broom::augment(transform_F_to_blom, newdata = hrs16_merged) %>%
  rename(Pblom = .fitted)

hrs16_merged <- hrs16_merged %>%
  labelled::set_variable_labels(Pblom = "Predicted Blom score (model from HCAP normal sample)")

hrs16_22_long <- broom::augment(transform_F_to_blom, newdata = hrs16_22_long) %>%
  rename(Pblom = .fitted)

hrs16_22_long <- hrs16_22_long %>%
  labelled::set_variable_labels(Pblom = "Predicted Blom score (model from HCAP normal sample)")

saveRDS(hrs16_cog_norm, here::here("R_objects", "A7_052_hrs16_cog_norm.rds"))
saveRDS(hrs16_merged,   here::here("R_objects", "A7_052_hrs16_merged.rds"))
saveRDS(hrs16_22_long,  here::here("R_objects", "A7_052_hrs16_22_long.rds"))


