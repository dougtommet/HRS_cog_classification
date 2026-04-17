

hrs16_merged <- readRDS(here::here("R_objects", "A7_050_hrs16_merged.rds"))

hrs16_cog_norm <- hrs16_merged %>%
  filter(normexcld==0) %>%
  mutate(HCAP16WGTR0 = case_when(is.na(HCAP16WGTR) ~ 0,
                                 TRUE ~ HCAP16WGTR))


#### Blom transform the factor score
# Blom transform the factor score in the norming sample
hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(f_blom = rcompanion::blom(F)
  )



# To go from factor score to blom score
# create cubic splines for F
q <- quantile(hrs16_cog_norm$F, c(.05, .35, .65, .95), na.rm=TRUE)

# Create the cubic splines for F in the norming sample
hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(k1 = q[[1]],
         k2 = q[[2]],
         k3 = q[[3]],
         k4 = q[[4]],
         spF1 = F,
         spF2 = case_when(!is.na(F) ~
                            (pmax((F - k1)^3, 0) -
                               (k4 - k3)^-1 *
                               (pmax((F - k3)^3, 0) * (k4 - k1) -
                                  pmax((F - k4)^3, 0) * (k3 - k1))) /
                            (k4 - k1)^2),
         spF3 = case_when(!is.na(F) ~
                            (pmax((F - k2)^3, 0) -
                               (k4 - k3)^-1 *
                               (pmax((F - k3)^3, 0) * (k4 - k2) -
                                  pmax((F - k4)^3, 0) * (k3 - k2))) /
                            (k4 - k1)^2)
  ) %>%
  select(-k1, -k2, -k3, -k4)
# Estimate the model to predict f_blom from F (using cubic splines)
transform_F_to_blom <- glm(f_blom ~ spF1 + spF2 + spF3, data = hrs16_cog_norm)

# Get the predicted f_blom values and merge into the norming sample
hrs16_cog_norm <- broom::augment(transform_F_to_blom, newdata = hrs16_cog_norm) %>%
  rename(Pblom = .fitted)

hrs16_cog_norm <- hrs16_cog_norm %>%
  labelled::set_variable_labels(spF1 = "F spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spF2 = "F spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spF3 = "F spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(Pblom = "Predicted Blom score (model from HCAP normal sample)")

# Create the cubic splines for F in the full sample
hrs16_merged <- hrs16_merged %>%
  mutate(k1 = q[[1]],
         k2 = q[[2]],
         k3 = q[[3]],
         k4 = q[[4]],
         spF1 = F,
         spF2 = case_when(!is.na(F) ~
                            (pmax((F - k1)^3, 0) -
                               (k4 - k3)^-1 *
                               (pmax((F - k3)^3, 0) * (k4 - k1) -
                                  pmax((F - k4)^3, 0) * (k3 - k1))) /
                            (k4 - k1)^2),
         spF3 = case_when(!is.na(F) ~
                            (pmax((F - k2)^3, 0) -
                               (k4 - k3)^-1 *
                               (pmax((F - k3)^3, 0) * (k4 - k2) -
                                  pmax((F - k4)^3, 0) * (k3 - k2))) /
                            (k4 - k1)^2)
  ) %>%
  select(-k1, -k2, -k3, -k4)

# Get the predicted f_blom values and merge into the full sample
hrs16_merged <- broom::augment(transform_F_to_blom, newdata = hrs16_merged) %>%
  rename(Pblom = .fitted)

hrs16_merged <- hrs16_merged %>%
  labelled::set_variable_labels(spF1 = "F spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spF2 = "F spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spF3 = "F spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(Pblom = "Predicted Blom score (model from HCAP normal sample)")


########

# foo_glm <- glm(Pblom ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 +
#                  x1x4 + x1x5 + x1x6 + x1x7 + x2x4 + x2x5 + x2x6 + x2x7 + x3x4 + x3x5 + x3x6 + x3x7 +
#                  x4x5 + x4x6 + x4x7 +
#                  x5x7 +
#                  x6x7,
#                data = hrs16_cog_norm)
# foo_lm <- lm(Pblom ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 +
#                x1x4 + x1x5 + x1x6 + x1x7 + x2x4 + x2x5 + x2x6 + x2x7 + x3x4 + x3x5 + x3x6 + x3x7 +
#                x4x5 + x4x6 + x4x7 +
#                x5x7 +
#                x6x7,
#              data = hrs16_cog_norm)


svy_design <- svydesign(
  ids = ~SECU,
  strata = ~STRATUM,
  weights = ~HCAP16WGTR0,
  data = hrs16_cog_norm,
  nest = TRUE
)


foo_glm<- svyglm(Pblom ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 +
                   x1x4 + x1x5 + x1x6 + x1x7 + x2x4 + x2x5 + x2x6 + x2x7 + x3x4 + x3x5 + x3x6 + x3x7 +
                   x4x5 + x4x6 + x4x7 +
                   x5x7 +
                   x6x7,
                 design = svy_design)

goo_glm <- broom::tidy(foo_glm)
# goo_lm <- broom::tidy(foo_lm)


hrs16_cog_norm <- broom::augment(foo_glm, newdata = hrs16_cog_norm, type.predict = "response") %>%
  rename(Eblom = .fitted)

hrs16_cog_norm <- hrs16_cog_norm %>%
  labelled::set_variable_labels(Eblom = "Demographically-adjusted Blom score")

hrs16_merged <- broom::augment(foo_glm, newdata = hrs16_merged, type.predict = "response") %>%
  rename(Eblom = .fitted)

hrs16_merged <- hrs16_merged %>%
  labelled::set_variable_labels(Eblom = "Demographically-adjusted Blom score")


broom::glance(foo_glm) %>%
  mutate(r2 = 1- deviance/null.deviance)
# R2 = .366
sd(hrs16_cog_norm$Pblom, na.rm=TRUE)
# sd = 0.9987769

# (0.9987769 * sqrt(1-.366)) = 0.7952673

hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(
    TF = 50 + 10* ((Pblom - Eblom)/(0.9987769 * sqrt(1-.366)) )
  )
hrs16_cog_norm <- hrs16_cog_norm %>%
  labelled::set_variable_labels(TF = "T-scaled F residual")

hrs16_merged <- hrs16_merged %>%
  mutate(
    TF = 50 + 10* ((Pblom - Eblom)/(0.9987769 * sqrt(1-.366)))
  )
hrs16_merged <- hrs16_merged %>%
  labelled::set_variable_labels(TF = "T-scaled F residual")



# gen T`y'= 50+10*((P`y'_blom-E`y'_blom)/(`sd_P`y'_blom'*sqrt(1-`r2`y''))) // SEE
# reg f_blom (c.spage1 c.spage2 c.spage3)##(c.female c.black c.hisp c.schlyrs) c.female#(c.black c.hisp c.schlyrs) c.schlyrs#(c.black c.hisp) [fw=hcap16wgt] if normexcld==0

saveRDS(hrs16_merged,   here::here("R_objects", "A7_055_hrs16_merged.rds"))
saveRDS(hrs16_cog_norm, here::here("R_objects", "A7_055_hrs16_cog_norm.rds"))

saveRDS(foo_glm,        here::here("R_objects", "A7_055_foo_lm.rds"))

