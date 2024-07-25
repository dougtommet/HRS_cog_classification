
h16d_r <- readRDS(fs::path(r_objects_folder, "005_h16d_r.rds"))

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



hrs16_func <- h16d_r %>%
  select(HHID, PN, PSUBHH,
         PD102,
         PD502, PD505, PD506, PD507, PD508, PD509, PD510,
         PD511, PD512, PD513, PD514, PD515, PD516, PD517, PD518, PD519, PD520,
         PD521, PD522, PD523, PD524, PD525, PD526, PD527, PD528, PD529, PD530,
         PD531, PD532, PD533, PD534, PD535, PD536, PD537, PD538, PD539, PD540,
         PD541, PD542, PD543, PD544, PD545, PD546, PD547, PD548, PD549, PD550,
         PD551, PD552, PD553)

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
saveRDS(hrs16_func, fs::path(r_objects_folder, "010_hrs16_func.rds"))

