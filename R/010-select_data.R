

tracker <- readRDS(fs::path(r_objects_folder, "005_tracker.rds"))
h16a_r <- readRDS(fs::path(r_objects_folder, "005_h16a_r.rds"))
h16d_r <- readRDS(fs::path(r_objects_folder, "005_h16d_r.rds"))
h16g_r <- readRDS(fs::path(r_objects_folder, "005_h16g_r.rds"))
normexcld <- readRDS(fs::path(r_objects_folder, "005_normexcld.rds"))

hrs16_cog <- h16d_r %>%
  select(HHID, PN, PSUBHH,
         # starts_with("PD182M"), # using PD174 instead
         PD174,
         # starts_with("PD183M"), # using PD184 instead
         PD184,
         PD151, PD152, PD153, PD154,
         PD124, PD129,
         PD155, PD156,
         PD157, PD158,
         PD142, PD143, PD144, PD145, PD146,
         PD196, PD198, PNSSCORE)


# RNJ Added PD101 2025-07-21
hrs16_func <- h16d_r %>%
  select(HHID, PN, PSUBHH,
         PD102, PD101,
         PD502, PD505, PD506, PD507, PD508, PD509, PD510,
         PD511, PD512, PD513, PD514, PD515, PD516, PD517, PD518, PD519, PD520,
         PD521, PD522, PD523, PD524, PD525, PD526, PD527, PD528, PD529, PD530,
         PD531, PD532, PD533, PD534, PD535, PD536, PD537, PD538, PD539, PD540,
         PD541, PD542, PD543, PD544, PD545, PD546, PD547, PD548, PD549, PD550,
         PD551, PD552, PD553)


# RNJ added PG013 2025-07-26
hrs16_iadl <- h16g_r %>%
  select(HHID, PN, PSUBHH,
         PG013,
         PG014, PG021, PG023, PG030, PG040, PG041, PG044, PG047, PG050, PG051, PG059)

tracker_demo <- tracker %>%
  select(HHID, PN, PSUBHH,
         GENDER, HISPANIC, RACE, SCHLYRS, DEGREE)

# 2025-07-26
# Rich Jones Added this.I will replace missing (NA, 99) on SCHLYRS with
# a value drawn using predictive mean matching using DEGREE, GENDER, HISPANIC,
# and RACE and BIRTHYR as predictors. There are many people who have 99 for SCHLYRS but
# have information on educational attainment in the variable DEGREE.
# The new variable is called SCHLYRSimp
library(dplyr)
library(mice)
# Prepare data: replace 99 with NA in SCHLYRS
tracker_demo <- tracker %>%
  select(HHID, PN, PSUBHH,
         GENDER, HISPANIC, RACE, SCHLYRS, DEGREE, BIRTHYR) %>%
  mutate(SCHLYRS = na_if(SCHLYRS, 99))  # treat 99 as NA
# Set up predictors
impute_vars <- c("SCHLYRS", "DEGREE", "GENDER", "HISPANIC", "RACE", "BIRTHYR")
# Subset for imputation
imp_data <- tracker_demo %>% select(all_of(impute_vars))
# Run mice with 1 imputation using PMM
imp_result <- mice(imp_data, m = 1, method = "pmm", seed = 2 , printFlag = FALSE)
# Extract completed data
completed <- complete(imp_result, 1)
# Replace imputed SCHLYRS into tracker_demo
tracker_demo$SCHLYRSimp <- completed$SCHLYRS
attr(tracker_demo$SCHLYRSimp, "label") <- "Number of years in school (imputed)"
# table(tracker_demo$SCHLYRS, useNA = "ifany")
# table(tracker_demo$SCHLYRSimp, useNA = "ifany")







h16a_r_demo <- h16a_r %>%
  select(HHID, PN, PSUBHH,
         PA019)

hrs16_cog <- hrs16_cog %>%
  left_join(tracker_demo, by = c("HHID" = "HHID", "PN" = "PN", "PSUBHH" = "PSUBHH")) %>%
  left_join(h16a_r_demo, by = c("HHID" = "HHID", "PN" = "PN", "PSUBHH" = "PSUBHH")) %>%
  left_join(normexcld, by = c("HHID" = "hhid", "PN" = "pn"))





# From dropbox/work/pitch/posted/analysis/hrs coda/extract hrscogdata170322.do
# callhrs_lg , datadrive($d/Laura/PITCH/HRS) substublist(2014) h(h)
# ids(HHID PN) newmetadata varstopull(ORC104
#   OD104 OD124 OD129 OD142 OD143 OD144 OD145 OD146 OD184 OD174 OD151
#   OD152 OD153 OD154 OD155 OD156 OD157 OD158
#   OD178 OD179 OD180 OD196 OD198 OD250 OD251 OD252 OD253 OD254 OD255 OD256
#   OD257 OD258 OD259 OD260 OD261 OD262 OD263 OD264   OD265
#   OD270 OD271 OD272 OD273 OD274 OD275 OD276 OD277 OD278
#   OD279 OD280 OD281 OD282 OD283 OD284 OVESCORE OVESCORESE
#   OA019 OB015 OB014 OB028 OX060_R OB091M	);

# These are the items from the list above that weren't selected
# OD104
#   OD198 OD250 OD251 OD252 OD253 OD254 OD255 OD256
#   OD257 OD258 OD259 OD260 OD261 OD262 OD263 OD264   OD265
#   OD270 OD271 OD272 OD273 OD274 OD275 OD276 OD277 OD278
#   OD279 OD280 OD281 OD282 OD283 OD284 OVESCORE OVESCORESE

saveRDS(hrs16_cog, fs::path(r_objects_folder, "010_hrs16_cog.rds"))
saveRDS(hrs16_iadl, fs::path(r_objects_folder, "010_hrs16_iadl.rds"))
saveRDS(hrs16_func, fs::path(r_objects_folder, "010_hrs16_func.rds"))

