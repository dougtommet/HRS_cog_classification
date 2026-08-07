
hrs16_cog_norm <- readRDS(here::here("R_objects", "A7_052_hrs16_cog_norm.rds"))
hrs16_merged   <- readRDS(here::here("R_objects", "A7_052_hrs16_merged.rds"))
hrs16_22_long  <- readRDS(here::here("R_objects", "A7_053_hrs16_22_long.rds"))


hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(x1   = xP1 ,
         x2   = xP2 ,
         x3   = xP3 ,
         x4   = xP4 ,
         x5   = xP5 ,
         x6   = xP6 ,
         x7   = xP7 ,
         x1x4 = xP1xP4 ,
         x1x5 = xP1xP5 ,
         x1x6 = xP1xP6 ,
         x1x7 = xP1xP7 ,
         x2x4 = xP2xP4 ,
         x2x5 = xP2xP5 ,
         x2x6 = xP2xP6 ,
         x2x7 = xP2xP7 ,
         x3x4 = xP3xP4 ,
         x3x5 = xP3xP5 ,
         x3x6 = xP3xP6 ,
         x3x7 = xP3xP7 ,
         x4x5 = xP4xP5 ,
         x4x6 = xP4xP6 ,
         x4x7 = xP4xP7 ,
         x5x7 = xP5xP7 ,
         x6x7 = xP6xP7
         )

hrs16_merged <- hrs16_merged %>%
  mutate(x1   = xP1 ,
         x2   = xP2 ,
         x3   = xP3 ,
         x4   = xP4 ,
         x5   = xP5 ,
         x6   = xP6 ,
         x7   = xP7 ,
         x1x4 = xP1xP4 ,
         x1x5 = xP1xP5 ,
         x1x6 = xP1xP6 ,
         x1x7 = xP1xP7 ,
         x2x4 = xP2xP4 ,
         x2x5 = xP2xP5 ,
         x2x6 = xP2xP6 ,
         x2x7 = xP2xP7 ,
         x3x4 = xP3xP4 ,
         x3x5 = xP3xP5 ,
         x3x6 = xP3xP6 ,
         x3x7 = xP3xP7 ,
         x4x5 = xP4xP5 ,
         x4x6 = xP4xP6 ,
         x4x7 = xP4xP7 ,
         x5x7 = xP5xP7 ,
         x6x7 = xP6xP7
  )

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
                   x1x4 + x1x5 + x1x6 + x1x7 +
                   x2x4 + x2x5 + x2x6 + x2x7 +
                   x3x4 + x3x5 + x3x6 + x3x7 +
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

hrs16_22_long_temp <- hrs16_22_long %>%
  filter(!is.na(x1))
hrs16_22_long_temp <- broom::augment(foo_glm, newdata = hrs16_22_long_temp, type.predict = "response") %>%
  rename(Eblom = .fitted)
hrs16_22_long_temp <- hrs16_22_long_temp %>%
  select(id, wave, Eblom)
hrs16_22_long <- hrs16_22_long %>%
  left_join(hrs16_22_long_temp, by = c("id", "wave"))

hrs16_22_long <- hrs16_22_long %>%
  labelled::set_variable_labels(Eblom = "Demographically-adjusted Blom score")



r2 <- broom::glance(foo_glm) %>%
  mutate(r2 = 1- deviance/null.deviance) %>%
  pull(r2)
# R2 = .366
# sd(hrs16_cog_norm$Pblom, na.rm=TRUE)
Pblom_sd <- Hmisc::wtd.var(hrs16_cog_norm$Pblom, hrs16_cog_norm$HCAP16WGTR0)^.5
# sd = 0.9728137
(Pblom_sd * sqrt(1-r2))
# (0.9728137 * sqrt(1-.366)) = 0.7745943

hrs16_cog_norm <- hrs16_cog_norm %>%
  mutate(
    TF = 50 + 10* ((Pblom - Eblom)/(Pblom_sd * sqrt(1-r2)) )
  )
hrs16_cog_norm <- hrs16_cog_norm %>%
  labelled::set_variable_labels(TF = "T-scaled F residual")

hrs16_merged <- hrs16_merged %>%
  mutate(
    TF = 50 + 10* ((Pblom - Eblom)/(Pblom_sd * sqrt(1-r2)))
  )
hrs16_merged <- hrs16_merged %>%
  labelled::set_variable_labels(TF = "T-scaled F residual")

hrs16_22_long <- hrs16_22_long %>%
  mutate(
    TF = 50 + 10* ((Pblom - Eblom)/(Pblom_sd * sqrt(1-r2)))
  )
hrs16_22_long <- hrs16_22_long %>%
  labelled::set_variable_labels(TF = "T-scaled F residual")

# gen T`y'= 50+10*((P`y'_blom-E`y'_blom)/(`sd_P`y'_blom'*sqrt(1-`r2`y''))) // SEE
# reg f_blom (c.spage1 c.spage2 c.spage3)##(c.female c.black c.hisp c.schlyrs) c.female#(c.black c.hisp c.schlyrs) c.schlyrs#(c.black c.hisp) [fw=hcap16wgt] if normexcld==0

saveRDS(hrs16_merged,   here::here("R_objects", "A7_055_hrs16_merged.rds"))
saveRDS(hrs16_cog_norm, here::here("R_objects", "A7_055_hrs16_cog_norm.rds"))
saveRDS(hrs16_22_long,  here::here("R_objects", "A7_055_hrs16_22_long.rds"))

saveRDS(foo_glm,        here::here("R_objects", "A7_055_foo_lm.rds"))

