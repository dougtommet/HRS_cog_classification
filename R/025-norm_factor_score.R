


hrs16_cog <- readRDS(fs::path(r_objects_folder, "020_hrs16_cog.rds"))
names(hrs16_cog)

# Get the "normal" sample from HCAP
hrs16_cog_norm <- hrs16_cog %>%
  filter(normexcld==0)

#### Center the demographic variables
# Get the centering values in the norming sample
# centering_info <- tribble(~variable, ~mean,
#                           "female", mean(hrs16_cog_norm$female, na.rm = TRUE),
#                           "black", mean(hrs16_cog_norm$black, na.rm = TRUE),
#                           "hisp", mean(hrs16_cog_norm$hisp, na.rm = TRUE),
#                           "SCHLYRS", mean(hrs16_cog_norm$SCHLYRS, na.rm = TRUE))

centering_info <- hrs16_cog_norm %>%
  select(female, black, hisp, SCHLYRS) %>%
  summarise_all(mean, na.rm=TRUE) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "mean")

get_centering_value <- function(x) {
  centering_info %>%
    filter(variable == {x}) %>%
    pull(mean)
}
# Center the variable in the norming sample
hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(cfemale  = female  - get_centering_value("female"),
         cblack   = black   - get_centering_value("black"),
         chisp    = hisp    - get_centering_value("hisp"),
         cschlyrs = SCHLYRS - get_centering_value("SCHLYRS")
  )

hrs16_cog_norm %>%
  select(cfemale, cblack, chisp, cschlyrs) %>%
  labelled::to_factor() %>%
  gtsummary::tbl_summary(statistic = list(
    gtsummary::all_continuous() ~ "{mean} ({sd})",
    gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
  ))

# Center the variable in the full sample
hrs16_cog <- hrs16_cog %>%
  mutate(cfemale  = female  - get_centering_value("female"),
         cblack   = black   - get_centering_value("black"),
         chisp    = hisp    - get_centering_value("hisp"),
         cschlyrs = SCHLYRS - get_centering_value("SCHLYRS")
  )

hrs16_cog %>%
  select(cfemale, cblack, chisp, cschlyrs) %>%
  labelled::to_factor() %>%
  gtsummary::tbl_summary(statistic = list(
    gtsummary::all_continuous() ~ "{mean} ({sd})",
    gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
  ))

#### Create the cubic splines for age
quantile(hrs16_cog_norm$rage, c(.05, .35, .65, .95))
# RNJ: used 70, 78, 86, 94 as knots

# Create the cubic splines for age in the norming sample
hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(k1 = 65,
         k2 = 70,
         k3 = 76,
         k4 = 85,
         spage1 = rage,
         spage2 = case_when(!is.na(rage) ~
                          (pmax((rage - k1)^3, 0) -
                             (k4 - k3)^-1 *
                             (pmax((rage - k3)^3, 0) * (k4 - k1) -
                                pmax((rage - k4)^3, 0) * (k3 - k1))) /
                            (k4 - k1)^2),
         spage3 = case_when(!is.na(rage) ~
                          (pmax((rage - k2)^3, 0) -
                             (k4 - k3)^-1 *
                             (pmax((rage - k3)^3, 0) * (k4 - k2) -
                                pmax((rage - k4)^3, 0) * (k3 - k2))) /
                            (k4 - k1)^2)
  ) %>%
  select(-k1, -k2, -k3, -k4)

# The created splines are the same as those from the rcs function
hrs16_cog_norm %>% select(spage1, spage2, spage3)
rms::rcs(hrs16_cog_norm$rage, 4)[1:10,]

# Create the cubic splines for age in the full sample
hrs16_cog <- hrs16_cog %>%
  mutate(k1 = 65,
         k2 = 70,
         k3 = 76,
         k4 = 85,
         spage1 = rage,
         spage2 = case_when(!is.na(rage) ~
                              (pmax((rage - k1)^3, 0) -
                                 (k4 - k3)^-1 *
                                 (pmax((rage - k3)^3, 0) * (k4 - k1) -
                                    pmax((rage - k4)^3, 0) * (k3 - k1))) /
                              (k4 - k1)^2),
         spage3 = case_when(!is.na(rage) ~
                              (pmax((rage - k2)^3, 0) -
                                 (k4 - k3)^-1 *
                                 (pmax((rage - k3)^3, 0) * (k4 - k2) -
                                    pmax((rage - k4)^3, 0) * (k3 - k2))) /
                              (k4 - k1)^2)
  ) %>%
  select(-k1, -k2, -k3, -k4)

hrs16_cog <- hrs16_cog %>%
  labelled::set_variable_labels(cfemale = "Female (centered to HCAP normal sample)") %>%
  labelled::set_variable_labels(cblack = "Black (centered to HCAP normal sample)") %>%
  labelled::set_variable_labels(chisp = "Hispanic (centered to HCAP normal sample)") %>%
  labelled::set_variable_labels(cschlyrs = "School years (centered to HCAP normal sample)") %>%
  labelled::set_variable_labels(spage1 = "Age spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spage2 = "Age spline 2 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spage3 = "Age spline 3 (from HCAP normal sample)")

hrs16_cog_norm <- hrs16_cog_norm %>%
  labelled::set_variable_labels(cfemale = "Female (centered to HCAP normal sample)") %>%
  labelled::set_variable_labels(cblack = "Black (centered to HCAP normal sample)") %>%
  labelled::set_variable_labels(chisp = "Hispanic (centered to HCAP normal sample)") %>%
  labelled::set_variable_labels(cschlyrs = "School years (centered to HCAP normal sample)") %>%
  labelled::set_variable_labels(spage1 = "Age spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spage2 = "Age spline 2 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spage3 = "Age spline 3 (from HCAP normal sample)")

#### Blom transform the factor score
# Blom transform the factor score in the norming sample
hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(f_blom = rcompanion::blom(F)
         )

F_mean <- hrs16_cog %>%
  filter(!is.na(normexcld)) %>%
  mutate(normexcld_f = factor(normexcld, labels = c("Included sample", "Excluded sample"))) %>%
  group_by(normexcld_f) %>%
  summarize(mean = mean(F, na.rm=TRUE))

hrs16_cog %>%
  filter(!is.na(normexcld)) %>%
  mutate(normexcld_f = factor(normexcld, labels = c("Included sample", "Excluded sample"))) %>%
  ggplot(aes(x = F)) +
  geom_histogram() +
  geom_vline(data = F_mean, aes(xintercept = mean), color = "red") +
  facet_wrap(~normexcld_f, ncol=1) +
  hrbrthemes::theme_ipsum()

hrs16_cog %>%
  filter(!is.na(normexcld)) %>%
  select(F, normexcld) %>%
  gtsummary::tbl_summary(by = normexcld)

# # To go from blom score to factor score - not sure if this direction is needed
# # create cubic splines for f_blom
# quantile(hrs16_cog_norm$f_blom, c(.05, .35, .65, .95), na.rm=TRUE)
#
# # Create the cubic splines for f_blom in the norming sample
# hrs16_cog_norm <- hrs16_cog_norm %>%
#   mutate(k1 = -1.64,
#          k2 = -0.38,
#          k3 =  0.38,
#          k4 =  1.64,
#          spfblom1 = f_blom,
#          spfblom2 = case_when(!is.na(f_blom) ~
#                               (pmax((f_blom - k1)^3, 0) -
#                                  (k4 - k3)^-1 *
#                                  (pmax((f_blom - k3)^3, 0) * (k4 - k1) -
#                                     pmax((f_blom - k4)^3, 0) * (k3 - k1))) /
#                               (k4 - k1)^2),
#          spfblom3 = case_when(!is.na(f_blom) ~
#                               (pmax((f_blom - k2)^3, 0) -
#                                  (k4 - k3)^-1 *
#                                  (pmax((f_blom - k3)^3, 0) * (k4 - k2) -
#                                     pmax((f_blom - k4)^3, 0) * (k3 - k2))) /
#                               (k4 - k1)^2)
#   ) %>%
#   select(-k1, -k2, -k3, -k4)
#
#
# transform_blom_to_F <- glm(F ~ spfblom1 + spfblom2 + spfblom3, data = hrs16_cog_norm)
# pred_F_df <- broom::augment(transform_blom_to_F) %>%
#   mutate(.rownames = as.numeric(.rownames)) %>%
#   select(.rownames, .fitted)
#
# hrs16_cog_norm <- hrs16_cog_norm %>%
#   mutate(.rownames = row_number()) %>%
#   left_join(pred_F_df, by = c(".rownames" = ".rownames")) %>%
#   rename(PF = .fitted)

# To go from factor score to blom score
# create cubic splines for F
quantile(hrs16_cog_norm$F, c(.05, .35, .65, .95), na.rm=TRUE)

# Create the cubic splines for F in the norming sample
hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(k1 = -1.21,
         k2 = -0.01,
         k3 =  0.53,
         k4 =  1.16,
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
hrs16_cog <- hrs16_cog %>%
  mutate(k1 = -1.21,
         k2 = -0.01,
         k3 =  0.53,
         k4 =  1.16,
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
hrs16_cog <- broom::augment(transform_F_to_blom, newdata = hrs16_cog) %>%
  rename(Pblom = .fitted)

hrs16_cog <- hrs16_cog %>%
  labelled::set_variable_labels(spF1 = "F spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spF2 = "F spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(spF3 = "F spline 1 (from HCAP normal sample)") %>%
  labelled::set_variable_labels(Pblom = "Predicted Blom score (model from HCAP normal sample)")


########
foo_glm <- glm(Pblom ~ spage1 + spage2 + spage3 + cfemale + cblack + chisp + cschlyrs +
             (spage1 + spage2 + spage3):(cfemale + cblack + chisp + cschlyrs)+
             cfemale:(cblack + chisp + cschlyrs) +
             cschlyrs:(cblack + chisp), data = hrs16_cog_norm)

foo_lm <- lm(Pblom ~ spage1 + spage2 + spage3 + cfemale + cblack + chisp + cschlyrs +
             (spage1 + spage2 + spage3):(cfemale + cblack + chisp + cschlyrs)+
             cfemale:(cblack + chisp + cschlyrs) +
             cschlyrs:(cblack + chisp), data = hrs16_cog_norm)
goo_glm <- broom::tidy(foo_glm)
goo_lm <- broom::tidy(foo_lm)


hrs16_cog_norm <- broom::augment(foo_lm, newdata = hrs16_cog_norm) %>%
  rename(Eblom = .fitted)

hrs16_cog_norm <- hrs16_cog_norm %>%
  labelled::set_variable_labels(Eblom = "Adjusted Blom score (from to HCAP normal sample)")

hrs16_cog <- broom::augment(foo_lm, newdata = hrs16_cog) %>%
  rename(Eblom = .fitted)

hrs16_cog <- hrs16_cog %>%
  labelled::set_variable_labels(Eblom = "Adjusted Blom score (from to HCAP normal sample)")




broom::glance(foo_lm)
# adj R-squared = 0.394
sd(hrs16_cog_norm$Pblom, na.rm=TRUE)
# sd = 0.998683

# (0.998683 * sqrt(1-.394)) = 0.7774348

hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(
    TF = 50 + 10* ((Pblom - Eblom)/(0.998683 * sqrt(1-.394)) )
           )
hrs16_cog_norm <- hrs16_cog_norm %>%
  labelled::set_variable_labels(TF = "T-scaled F score (from to HCAP normal sample)")

hrs16_cog <- hrs16_cog %>%
  mutate(
    TF = 50 + 10* ((Pblom - Eblom)/(0.998683 * sqrt(1-.394)) )
  )
hrs16_cog <- hrs16_cog %>%
  labelled::set_variable_labels(TF = "T-scaled F score (from to HCAP normal sample)")

hrs16_cog_norm %>%
  select(Pblom, Eblom, TF) %>%
  gtsummary::tbl_summary()


hrs16_cog_norm %>%
  ggplot(aes(x = Pblom, y = Eblom)) +
  geom_point() +
  geom_smooth() +
  ylim(c(-4, 3)) +
  xlim(c(-4, 3)) +
  theme(aspect.ratio=1)

hrs16_cog_norm %>%
  ggplot(aes(x = Pblom, y = TF)) +
  geom_point()

# gen T`y'= 50+10*((P`y'_blom-E`y'_blom)/(`sd_P`y'_blom'*sqrt(1-`r2`y''))) // SEE
# reg f_blom (c.spage1 c.spage2 c.spage3)##(c.female c.black c.hisp c.schlyrs) c.female#(c.black c.hisp c.schlyrs) c.schlyrs#(c.black c.hisp) [fw=hcap16wgt] if normexcld==0

saveRDS(hrs16_cog, fs::path(r_objects_folder, "025_hrs16_cog.rds"))
saveRDS(hrs16_cog_norm, fs::path(r_objects_folder, "025_hrs16_cog_norm.rds"))
saveRDS(centering_info, fs::path(r_objects_folder, "025_centering_info.rds"))
saveRDS(foo_lm, fs::path(r_objects_folder, "025_foo_lm.rds"))

