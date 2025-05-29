


hrs16_cog  <- readRDS(fs::path(r_objects_folder, "025_hrs16_cog.rds"))
hrs16_iadl <- readRDS(fs::path(r_objects_folder, "012_hrs16_iadl.rds"))
hrs16_func <- readRDS(fs::path(r_objects_folder, "013_hrs16_func.rds"))
hrshcap    <- readRDS(fs::path(r_objects_folder, "014_hrshcap.rds"))


# Using the n=3496 sample
hcap_cog <- hrs16_cog %>% filter(!is.na(normexcld))

hcap <- hcap_cog %>%
  left_join(hrs16_iadl, by = c("HHID" = "HHID", "PN" = "PN")) %>%
  left_join(hrs16_func, by = c("HHID" = "HHID", "PN" = "PN")) %>%
  left_join(hrshcap, by = c("HHID" = "hhid", "PN" = "pn"))

# Filtering to the nonimputed sample (n=2993)
hcap <- hcap %>%
  filter(imputed_flag==1)

# quantile(hcap_cog$TF, probs = c(.15, .35), na.rm=TRUE)
#
# hcap <- hcap %>%
#   mutate(cog_imp = case_when(TF <36.0 ~ 2,
#                              TF < 43.3 ~ 1,
#                              !is.na(TF) ~ 0))


algorithm_thresholds <- function(df){
  df <- df %>%
    mutate(cog_threshold = case_when(TF <36.0 ~ 2,
                                     TF < 43.3 ~ 1,
                                     !is.na(TF) ~ 0),
           iadl_threshold = case_when(cog_threshold==2 & iadl_imp>0 ~ 1,
                                      cog_threshold==2 & iadl_imp==0 ~ 0,
                                      cog_threshold==1 & iadl_imp>0 ~ 1,
                                      cog_threshold==1 & iadl_imp==0 ~ 0),
           jorm_threshold = case_when(vs3jormsc>= 3.4 ~ 2,
                                      vs3jormsc > 3.0 &  vs3jormsc < 3.4 ~ 1,
                                      vs3jormsc <= 3.0  ~ 0)
    )

  df
}

# v1_algorithm <- function(df) {
#   df <- df %>%
#     mutate(dx_v1 = case_when(cog_threshold==2 & iadl_threshold==1 ~ 2,
#                              cog_threshold==2 & iadl_threshold==0 & self_concerns==1 ~ 2,
#                              cog_threshold==2 & iadl_threshold==0 & self_concerns==0 ~ 1,
#                              cog_threshold==1 & iadl_threshold==1 ~ 1,
#                              cog_threshold==1 & iadl_threshold==0 & self_concerns==1 ~ 1,
#                              cog_threshold==1 & iadl_threshold==0 & self_concerns==0 ~ 0,
#                              cog_threshold==0 ~ 0,
#                              is.na(TF) & jorm_threshold==2 ~ 2,
#                              is.na(TF) & jorm_threshold==1 ~ 1,
#                              is.na(TF) & jorm_threshold==0 ~ 0
#     ))
#
#   df <- df %>%
#     labelled::set_variable_labels(dx_v1 = "Algorithm (V1) Dx") %>%
#     labelled::set_value_labels(dx_v1 = c("Normal" = 0, "MCI" = 1, "Dementia" = 2))
#
#   df
# }

v1_algorithm <- function(df) {
  df <- df %>%
    mutate(dx_v1 = case_when(
      cog_threshold == 2 & iadl_threshold == 1 ~ 2,
      cog_threshold == 2 & iadl_threshold == 0 & as.numeric(haven::zap_labels(self_concerns)) == 1 ~ 2,
      cog_threshold == 2 & iadl_threshold == 0 & as.numeric(haven::zap_labels(self_concerns)) == 0 ~ 1,
      cog_threshold == 1 & iadl_threshold == 1 ~ 1,
      cog_threshold == 1 & iadl_threshold == 0 & as.numeric(haven::zap_labels(self_concerns)) == 1 ~ 1,
      cog_threshold == 1 & iadl_threshold == 0 & as.numeric(haven::zap_labels(self_concerns)) == 0 ~ 0,
      cog_threshold == 0 ~ 0,
      is.na(TF) & jorm_threshold == 2 ~ 2,
      is.na(TF) & jorm_threshold == 1 ~ 1,
      is.na(TF) & jorm_threshold == 0 ~ 0
    ))

  df <- df %>%
    labelled::set_variable_labels(dx_v1 = "Algorithm (V1) Dx") %>%
    labelled::set_value_labels(dx_v1 = c("Normal" = 0, "MCI" = 1, "Dementia" = 2))

  df
}


v1_flow_diagram <- function(df) {
  a1 <- df %>% filter(is.na(TF)) %>% nrow(); a1
  a2 <- df %>% filter(cog_threshold %in% c(0, 1)) %>% nrow(); a2
  b2 <- df %>% filter(cog_threshold %in% c(2)) %>% nrow(); b2
  b1 <- df %>% filter(is.na(TF) & jorm_threshold %in% c(0, 1)) %>% nrow(); b1
  b3 <- df %>% filter(is.na(TF) & jorm_threshold ==0) %>% nrow(); b3
  c4 <- df %>% filter(is.na(TF) & jorm_threshold ==1) %>% nrow(); c4
  c1 <- df %>% filter(is.na(TF) & jorm_threshold ==2) %>% nrow(); c1
  b4 <- df %>% filter(cog_threshold==1) %>% nrow(); b4
  c2 <- df %>% filter(cog_threshold==2 & iadl_threshold==1) %>% nrow(); c2
  c3 <- df %>% filter(cog_threshold==2 & iadl_threshold==0) %>% nrow(); c3
  d1 <- df %>% filter(cog_threshold==2 & iadl_threshold==0 & self_concerns==1) %>% nrow(); d1
  d2 <- df %>% filter(cog_threshold==2 & iadl_threshold==0 & self_concerns==0) %>% nrow(); d2
  c5 <- df %>% filter(cog_threshold==1 & iadl_threshold==1) %>% nrow(); c5
  c6 <- df %>% filter(cog_threshold==1 & iadl_threshold==0) %>% nrow(); c6
  d3 <- df %>% filter(cog_threshold==1 & iadl_threshold==0 & self_concerns==1) %>% nrow(); d3
  d4 <- df %>% filter(cog_threshold==1 & iadl_threshold==0 & self_concerns==0) %>% nrow(); d4
  c7 <- df %>% filter(cog_threshold==0) %>% nrow(); c7
  e1 <- df %>% filter(is.na(TF) & is.na(jorm_threshold)) %>% nrow(); e1
  e2 <- df %>% filter(dx_v1==2) %>% nrow(); e2
  e3 <- df %>% filter(dx_v1==1) %>% nrow(); e3
  e4 <- df %>% filter(dx_v1==0) %>% nrow(); e4

  flowdiagram <- list(a1 = a1,
                      a2 = a2,
                      b2 = b2,
                      b1 = b1,
                      b3 = b3,
                      c4 = c4,
                      c1 = c1,
                      b4 = b4,
                      c2 = c2,
                      c3 = c3,
                      d1 = d1,
                      d2 = d2,
                      c5 = c5,
                      c6 = c6,
                      d3 = d3,
                      d4 = d4,
                      c7 = c7,
                      e1 = e1,
                      e2 = e2,
                      e3 = e3,
                      e4 = e4
                      )

  flowdiagram
}


# v2_algorithm <- function(df) {
#   df <- df %>%
#     mutate(dx_v2 = case_when(cog_threshold==2 & iadl_threshold==1 ~ 2,
#                              cog_threshold==2 & iadl_threshold==0 & self_concerns==1 ~ 1,
#                              cog_threshold==2 & iadl_threshold==0 & self_concerns==0 ~ 1,
#                              cog_threshold==1 & iadl_threshold==1 ~ 1,
#                              cog_threshold==1 & iadl_threshold==0 & self_concerns==1 ~ 1,
#                              cog_threshold==1 & iadl_threshold==0 & self_concerns==0 ~ 0,
#                              cog_threshold==0 ~ 0,
#                              is.na(TF) & jorm_threshold==2 ~ 2,
#                              is.na(TF) & jorm_threshold==1 ~ 1,
#                              is.na(TF) & jorm_threshold==0 ~ 0
#                              )
#     )
#
#   df <- df %>%
#     labelled::set_variable_labels(dx_v2 = "Algorithm (V2) Dx") %>%
#     labelled::set_value_labels(dx_v2 = c("Normal" = 0, "MCI" = 1, "Dementia" = 2))
#
#   df
# }

# RNJ Corrected 2025-07-18
# Actually, I did not correct it, I left it as it is and changed the flow diagram to match.
# The issue is the rules below did not follow the April 2005 Flow diagram. That diagram
# had persons with missing cognition, and no evidence of impairment on Jorm, go through
# additional filters based on IADL/ADLS and self-rated concerns. Problem is, these
# items were missing for every observation with missing cognitive score and non-missing
# Jorm. So, we'll just go with the algorithm below, which sorts people with missing
# Cognition on the Jorm straight-up.
v2_algorithm <- function(df) {
  df <- df %>%
    mutate(dx_v2 = case_when(
      cog_threshold == 2 & iadl_threshold == 1 ~ 2,
      cog_threshold == 2 & iadl_threshold == 0 ~ 1,
      cog_threshold == 1 & iadl_threshold == 1 ~ 1,
      cog_threshold == 1 & iadl_threshold == 0 & as.numeric(haven::zap_labels(self_concerns)) == 1 ~ 1,
      cog_threshold == 1 & iadl_threshold == 0 & as.numeric(haven::zap_labels(self_concerns)) == 0 ~ 0,
      cog_threshold == 0 ~ 0,
      is.na(TF) & jorm_threshold == 2 ~ 2,
      is.na(TF) & jorm_threshold == 1 ~ 1,
      is.na(TF) & jorm_threshold == 0 ~ 0
    ))

  df <- df %>%
    labelled::set_variable_labels(dx_v2 = "Algorithm (V2) Dx") %>%
    labelled::set_value_labels(dx_v2 = c("Normal" = 0, "MCI" = 1, "Dementia" = 2))

  df
}


# RNJ Revised 2025-07-17
v2_flow_diagram <- function(df) {
  a1 <- df %>% filter(is.na(TF)) %>% nrow(); a1                                 # a1 - unknown cognitive status
  a2 <- df %>% filter(cog_threshold %in% c(0, 1)) %>% nrow(); a2                # a2 - not severe cognitive impairment
  b1 <- df %>% filter(cog_threshold %in% c(2)) %>% nrow(); b1                   # b1 - severe cognitive impairment
  b2 <- df %>% filter(cog_threshold==1) %>% nrow(); b2                          # b2 - moderate cognitive impairment
  b3 <- df %>% filter(cog_threshold==0) %>% nrow(); b3                          # b3 - not moderate (or severe) cog imp
  c1 <- df %>% filter(is.na(TF) & jorm_threshold %in% c(0, 1)) %>% nrow(); c1   # c1 - jorm < 3.4
  c2 <- df %>% filter(is.na(TF) & jorm_threshold ==0) %>% nrow(); c2            # c2 - jorm <= 3
  d1 <- df %>% filter(is.na(TF) & jorm_threshold ==2) %>% nrow(); d1            # d1 - jorm >=3.5
  d4 <- df %>% filter(is.na(TF) & jorm_threshold ==1) %>% nrow(); d4            # d4 - jorm (3-3.4), jorm_threshold == 1
  d2 <- df %>% filter(cog_threshold==2 & iadl_threshold==1) %>% nrow(); d2      # d2 - SCI, iadl impairment
  d3 <- df %>% filter(cog_threshold==2 & iadl_threshold==0) %>% nrow(); d3      # d3 - SCI, no iadl impairment
  d5 <- df %>% filter(cog_threshold==1 & iadl_threshold==1) %>% nrow(); d5      # d5 - moderate cog impairment, iadl_threshold IS THIS THE RIGHT IADL
  d6 <- df %>% filter(cog_threshold==1 & iadl_threshold==0) %>% nrow(); d6      # d6 - moderate, iadl no,
  e1 <- df %>% filter(cog_threshold==1 & iadl_threshold==0 &  as.numeric(haven::zap_labels(self_concerns))==1) %>% nrow(); e1
  e2 <- df %>% filter(cog_threshold==1 & iadl_threshold==0 &  as.numeric(haven::zap_labels(self_concerns))==0) %>% nrow(); e2
  f1 <- df %>% filter(is.na(TF) & is.na(jorm_threshold)) %>% nrow(); f1 # missing cognition and Jorm
  f2 <- df %>% filter(dx_v2==2) %>% nrow(); f2 # dementia This is a cheat!
  f3 <- df %>% filter(dx_v2==1) %>% nrow(); f3 # mci
  f4 <- df %>% filter(dx_v2==0) %>% nrow(); f4 # Normal

  flowdiagram <- list(a1 = a1,
                      a2 = a2,
                      b1 = b1,
                      b2 = b2,
                      b3 = b3,
                      c1 = c1,
                      c2 = c2,
                      d1 = d1,
                      d2 = d2,
                      d3 = d3,
                      d4 = d4,
                      d5 = d5,
                      d6 = d6,
                      e1 = e1,
                      e2 = e2,
                      f1 = f1,
                      f2 = f2,
                      f3 = f3,
                      f4 = f4
  )

  flowdiagram
}
hcap<- algorithm_thresholds(hcap)

hcap<- v1_algorithm(hcap)
# QSPtools::checkvar(hcap, dx_v1, cog_threshold, iadl_threshold, self_concerns, jorm_threshold)
#
# v1_flow_diagram(hcap)
hcap<- v2_algorithm(hcap)
QSPtools::checkvar(hcap, dx_v2, cog_threshold, iadl_threshold, self_concerns, jorm_threshold)
#
# v2_flow_diagram(hcap)

saveRDS(hcap, fs::path(r_objects_folder, "030_hcap.rds"))
saveRDS(algorithm_thresholds, fs::path(r_objects_folder, "030_algorithm_thresholds.rds"))
saveRDS(v1_algorithm, fs::path(r_objects_folder, "030_v1_algorithm.rds"))
saveRDS(v1_flow_diagram, fs::path(r_objects_folder, "030_v1_flow_diagram.rds"))
saveRDS(v2_algorithm, fs::path(r_objects_folder, "030_v2_algorithm.rds"))
saveRDS(v2_flow_diagram, fs::path(r_objects_folder, "030_v2_flow_diagram.rds"))

# Checks on v2_flow diagram and v2_algorithm
flow <- v2_flow_diagram(hcap)
flow
# sums to 2993
flow$a1+flow$b1+flow$a2
(flow$f1+flow$d1+flow$c1)+flow$b1+flow$a2
(flow$f1+flow$d1+flow$c1)+(flow$d2+flow$d3)+flow$a2
(flow$f1+flow$d1+flow$c1)+(flow$d2+flow$d3)+(flow$b2+flow$b3)
(flow$f1+flow$d1+(flow$d4+flow$c2))+(flow$d2+flow$d3)+(flow$b2+flow$b3)
(flow$f1+flow$d1+(flow$d4+flow$c2))+(flow$d2+flow$d3)+((flow$d5+flow$d6)+flow$b3)
(flow$f1+flow$d1+(flow$d4+flow$c2))+(flow$d2+flow$d3)+((flow$d5+(flow$e1+flow$e2+1))+flow$b3)
flow$f1+flow$f2+flow$f3+flow$f4+1
# evaluates to 0
flow$f2-(flow$d1+flow$d2) # dementia
flow$f3-(flow$d3+flow$d4+flow$d5+flow$e1)
flow$f4-(flow$e2+flow$b3+flow$c2)




