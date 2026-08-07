
hrs16_22_long <- readRDS(here::here("R_objects", "A7_052_hrs16_22_long.rds"))

hrs16_22_merged <- readRDS(here::here("R_objects", "A7_005_hrs16_22_merged.rds"))

# Pivoting the x variables longer in two steps
simple_long <- hrs16_22_merged %>%
  select(id, matches("^x(P|Q|R|S)\\d$")) %>%
  pivot_longer(
    cols = -id,
    names_to = c("wave", ".value"),
    names_pattern = "^x(P|Q|R|S)(\\d)$"
  ) %>%
  rename_with(~ paste0("x", .), -c(id, wave))

interaction_long <- hrs16_22_merged %>%
  select(id, matches("^x(P|Q|R|S)\\dx(P|Q|R|S)\\d$")) %>%
  # rename columns to standard format: xP1xP4 -> P_x1x4
  rename_with(
    ~ stringr::str_replace_all(., "^x(P|Q|R|S)(\\d)x(?:P|Q|R|S)(\\d)$", "\\1_x\\2x\\3")
  ) %>%
  pivot_longer(
    cols = -id,
    names_to = c("wave", ".value"),
    names_pattern = "^(P|Q|R|S)_(.+)$"
  )

foo_long <- left_join(simple_long,
                      interaction_long,
                      by = c("id", "wave"))

foo_jorm <- hrs16_22_merged %>%
  select(id, rPjorm, rQjorm, rRjorm, rSjorm) %>%
  pivot_longer(
    cols = -id,
    names_to = c("wave"),
    names_pattern = "^r(P|Q|R|S)jorm$",
    values_to = "jorm"
  )

foo_d102 <- hrs16_22_merged %>%
  select(id, PD102_imp, QD102_imp, RD102_imp, SD102_imp) %>%
  pivot_longer(
    cols = -id,
    names_to = c("wave"),
    names_pattern = "^(P|Q|R|S)D102_imp$",
    values_to = "D102_imp"
  )


hrs16_22_long <- hrs16_22_long %>%
  left_join(foo_long, by = c("id", "wave")) %>%
  left_join(foo_jorm, by = c("id", "wave")) %>%
  left_join(foo_d102, by = c("id", "wave"))

saveRDS(hrs16_22_long, here::here("R_objects", "A7_053_hrs16_22_long.rds"))



