
# Chat-extracted please double check
# ------------------------------------------------------------------------------
# Variable Assumptions for Applying Wu et al. (2013) Dementia Prediction Model
#
# Wu Table 1 Term                | Assumed Variable in `df`
# -------------------------------|------------------------------
# Immediate recall               | word_immed         (0–10)
# Delayed recall                 | word_delay         (0–10)
# TICS score                     | tics               (0–13)
# IQCODE (proxy only)            | iqcode             (1–5)
# Proxy memory score (proxy)     | proxy_mem          (1–5)
# Proxy respondent indicator     | proxy              (1 = yes, 0 = no)
# Age                            | age                (years)
# Sex (male)                     | male               (1 = male, 0 = female)
# Race (Black)                   | black              (1 = Black, 0 = White)
# Dementia status (for recal.)   | dementia           (1 = dementia, 0 = no)
#
# recal ~ recalibration
# but if we had dementia status, why would we need the model?
#
# Notes:
# - IQCODE and proxy_mem are used only when proxy == 1
# - Direct cognitive scores (word_immed, word_delay, tics) are used only when proxy == 0
# - Age should be centered at 70 as in Wu et al.'s model
# - Wu et al.'s model was developed using non-Hispanic individuals aged 70+
# ------------------------------------------------------------------------------


# Step 1: Center variables
df <- df %>%
  mutate(
    age_c = age - 70,
    iqcode_c = ifelse(proxy == 1, iqcode - 5, 0),
    proxy_mem_c = ifelse(proxy == 1, proxy_mem - 5, 0),
    word_immed_sq = word_immed^2,
    tics_sq = tics^2,
    word_delay_male = word_delay * male,
    iqcode_male = iqcode_c * male,
    word_delay_age = word_delay * age_c,
    proxy_age = proxy * age_c
  )

# Step 2: Compute linear predictor (excluding intercept)
df$lp_wu <- with(df, 
  0.933 * word_immed +
  (-0.266) * word_immed_sq +
  (-0.797) * word_delay +
  (-1.075) * tics +
  0.043 * tics_sq +
  2.220 * iqcode_c +
  1.096 * proxy_mem_c +
  1.889 * proxy +
  (-0.854) * male +
  0.095 * age_c +
  (-0.695) * black +
  0.543 * word_delay_male +
  1.551 * iqcode_male
)

# Step 3: Intercept recalibration (optional)
# Fit logistic regression with offset = Wu linear predictor
model_recal <- glm(dementia ~ offset(lp_wu), data = df, family = binomial())

# Step 4: Predict final probability using recalibrated intercept
beta0_star <- coef(model_recal)[1]
df$p_dementia <- plogis(beta0_star + df$lp_wu)

# View recalibrated probability estimates
head(df$p_dementia)
