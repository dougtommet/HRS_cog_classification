

# Using the n=3496 sample
hrs16_merged <- readRDS(here::here("R_objects", "A7_055_hrs16_merged.rds"))
hrs16_22_long <- readRDS(here::here("R_objects", "A7_055_hrs16_22_long.rds"))

hrs16_merged <- hrs16_merged %>%
  mutate(jorm = rPjorm,
         D102 = PD102_imp)
hrs16_22_long <- hrs16_22_long %>%
  mutate(D102 = D102_imp)

algorithm_thresholds <- function(df){
  df <- df %>%
    mutate(cog_threshold = case_when(TF <37 ~ 2,
                                     TF < 48 ~ 1,
                                     !is.na(TF) ~ 0),
           jorm_threshold = case_when(jorm>= 3.4 ~ 2,
                                      jorm > 3.0 &  jorm < 3.4 ~ 1,
                                      jorm <= 3.0  ~ 0)
    )

  df
}

v1_algorithm <- function(df) {
  df <- df %>%
    mutate(dx_v1 = case_when(
      cog_threshold == 2 & D102 %in% c(-8, 3, 8, 9) ~ 3,
      cog_threshold == 2 & D102 %in% c(1, 2) ~ 2,
      cog_threshold == 1 & D102 %in% c(-8, 3, 8, 9) ~ 2,
      cog_threshold == 1 & D102 %in% c(1, 2) ~ 1,
      cog_threshold == 0 ~ 1,
      is.na(TF) & jorm_threshold == 2 ~ 3,
      is.na(TF) & jorm_threshold == 1 ~ 2,
      is.na(TF) & jorm_threshold == 0 ~ 1
    ))

  df <- df %>%
    labelled::set_variable_labels(dx_v1 = "Algorithm (V1) Dx") %>%
    labelled::set_value_labels(dx_v1 = c("Normal" = 1, "MCI" = 2, "Dementia" = 3))

  df
}

v1_flow_diagram <- function(df) {
  a1 <- df %>% nrow(); a1
  b1 <- df %>% filter(is.na(TF)) %>% nrow(); b1
  b2 <- df %>% filter(cog_threshold %in% c(2)) %>% nrow(); b2
  b3 <- df %>% filter(cog_threshold %in% c(0, 1)) %>% nrow(); b3
  b4 <- df %>% filter(cog_threshold %in% c(1)) %>% nrow(); b4
  b5 <- df %>% filter(cog_threshold %in% c(0)) %>% nrow(); b5
  c1 <- df %>% filter(is.na(TF) & jorm_threshold ==2) %>% nrow(); c1
  c2 <- df %>% filter(is.na(TF) & jorm_threshold %in% c(0, 1)) %>% nrow(); c2
  c3 <- df %>% filter(is.na(TF) & jorm_threshold ==1) %>% nrow(); c3
  c4 <- df %>% filter(is.na(TF) & jorm_threshold ==0) %>% nrow(); c4
  d1 <- df %>% filter(cog_threshold==2 & D102 %in% c(3, 8, 9)) %>% nrow(); d1
  d2 <- df %>% filter(cog_threshold==2 & D102 %in% c(1, 2)) %>% nrow(); d2
  d3 <- df %>% filter(cog_threshold==1 & D102 %in% c(3, 8, 9)) %>% nrow(); d3
  d4 <- df %>% filter(cog_threshold==1 & D102 %in% c(1, 2)) %>% nrow(); d4

  e1 <- df %>% filter(dx_v1==1) %>% nrow(); e1
  e2 <- df %>% filter(dx_v1==2) %>% nrow(); e2
  e3 <- df %>% filter(dx_v1==3) %>% nrow(); e3

  flowdiagram <- list(a1 = a1,

                      b1 = b1,
                      b2 = b2,
                      b3 = b3,
                      b4 = b4,
                      b5 = b5,
                      c1 = c1,
                      c2 = c2,
                      c3 = c3,
                      c4 = c4,
                      d1 = d1,
                      d2 = d2,
                      d3 = d3,
                      d4 = d4,
                      e1 = e1,
                      e2 = e2,
                      e3 = e3
  )

  flowdiagram
}

hrs16_merged<- algorithm_thresholds(hrs16_merged)
hrs16_merged<- v1_algorithm(hrs16_merged)
QSPtools::checkvar(hrs16_merged, dx_v1, cog_threshold, D102, jorm_threshold)

hrs16_22_long <- algorithm_thresholds(hrs16_22_long)
hrs16_22_long <- v1_algorithm(hrs16_22_long)
QSPtools::checkvar(hrs16_22_long, dx_v1, cog_threshold, D102, jorm_threshold)

hrs16_flow_diagram_values <- v1_flow_diagram(hrs16_merged)
hcap_flow_diagram_values <-hrs16_merged %>%
  filter(inHCAP==1 ) %>%
  v1_flow_diagram()


saveRDS(hrs16_merged,                  here::here("R_objects", "A7_075_hrs16_merged.rds"))
saveRDS(hrs16_22_long,                  here::here("R_objects", "A7_075_hrs16_22_long.rds"))
saveRDS(algorithm_thresholds,          here::here("R_objects", "A7_075_algorithm_thresholds.rds"))
saveRDS(v1_algorithm,                  here::here("R_objects", "A7_075_v1_algorithm.rds"))
saveRDS(hcap_flow_diagram_values,      here::here("R_objects", "A7_075_hcap_flow_diagram_values.rds"))
saveRDS(hrs16_flow_diagram_values,      here::here("R_objects", "A7_075_hrs16_flow_diagram_values.rds"))

foo <- hrs16_22_long %>%
  filter(inHRS==1) %>%
  select(HHID, PN, wave, TF, jorm, D102, dx_v1)
write_csv(foo, here::here("Data", "Dx_for_LK-2026-06-11.csv"))

