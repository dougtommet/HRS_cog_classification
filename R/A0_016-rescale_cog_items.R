
hrs16_cog <- readRDS(here::here("R_objects", "A0_015_hrs16_cog.rds"))
hrs18_cog <- readRDS(here::here("R_objects", "A0_015_hrs18_cog.rds"))
hrs20_cog <- readRDS(here::here("R_objects", "A0_015_hrs20_cog.rds"))
hrs22_cog <- readRDS(here::here("R_objects", "A0_015_hrs22_cog.rds"))



rescale_cog_fx <- function(df, x, ref_df, ref_letter) {

  minmax <- function(var, ref_var) {
    ref <- if (!is.null(ref_var)) ref_var else var

    sd_ref  <- sd(ref_var, na.rm = TRUE)
    min_ref <- min(ref_var, na.rm = TRUE)
    max_ref <- max(ref_var, na.rm = TRUE)

    c_n <- (5  / (8 * 1000)) * sd_ref
    c_d <- (10 / (8 * 1000)) * sd_ref

    (var - min_ref + c_n) / (max_ref - min_ref + c_d)
  }

  vars_to_transform <- c("vdlfl1", "vdwdimm", "vdwddel", "vdexf7")

  for (var in vars_to_transform) {

    df_var <- paste0("r", x, var)

    if (df_var %in% names(df)) {
      new_var <- paste0(df_var, "z")
      ref_var <- paste0("r", ref_letter, var)

      df[[new_var]] <- minmax(df[[df_var]], ref_var = ref_df[[ref_var]])

    }
  }

  df
}

hrs16_cog <- rescale_cog_fx(hrs16_cog, "P", ref_df = hrs16_cog, ref_letter = "P")
hrs18_cog <- rescale_cog_fx(hrs18_cog, "Q", ref_df = hrs16_cog, ref_letter = "P")
hrs20_cog <- rescale_cog_fx(hrs20_cog, "R", ref_df = hrs16_cog, ref_letter = "P")
hrs22_cog <- rescale_cog_fx(hrs22_cog, "S", ref_df = hrs16_cog, ref_letter = "P")

# Checking that the same test score will have the same z score across wave
# hrs16_cog %>%
#   select(rPvdlfl1, rPvdlfl1z) %>%
#   distinct() %>%
#   arrange(rPvdlfl1)
#
# hrs18_cog %>%
#   select(rQvdlfl1, rQvdlfl1z) %>%
#   distinct() %>%
#   arrange(rQvdlfl1)
#
# hrs20_cog %>%
#   select(rRvdlfl1, rRvdlfl1z) %>%
#   distinct() %>%
#   arrange(rRvdlfl1)



saveRDS(hrs16_cog, here::here("R_objects", "A0_016_hrs16_cog.rds"))
saveRDS(hrs18_cog, here::here("R_objects", "A0_016_hrs18_cog.rds"))
saveRDS(hrs20_cog, here::here("R_objects", "A0_016_hrs20_cog.rds"))
saveRDS(hrs22_cog, here::here("R_objects", "A0_016_hrs22_cog.rds"))

