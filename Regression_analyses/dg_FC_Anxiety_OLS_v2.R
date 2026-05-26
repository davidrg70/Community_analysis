# dg_FC_Anxiety_OLS_v2.R

# OLS model:
# Y (FC) = anxiety score + sex + anxiety * sex
# Additionally: Wald test: anxiety * gender interaction terms to see if it improves the model fit
rm(list = ls())

library(readxl)   
library(gtsummary)
library(lm.beta)  
library(interactions)
library(ggplot2)  
library(dplyr)
library(broom.helpers)
library(lmtest) 
library(emmeans)

directory <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/"
setwd(directory)
file <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/Analysis_6_August2025/gPPI4_SEED_L_AMY_Data_for_OLS.xlsx"

# --------- Load data ----------
df <- read_excel(file, col_names = TRUE, na = c("", "NA"))
df <- dplyr::rename(df, y_L_AMY_L_dAI = `y_L_AMY_L-dAI`)

# make a plot of the variables first:
ggplot(df, aes(total_MASC2_raw, y_L_AMY_L_dAI,
               color = factor(sex, levels = c(0,1), labels = c("Male","Female")))) +
  geom_point(size = 3, alpha = 0.8, na.rm = TRUE) +   # <-- bigger dots
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(x = "MASC-2 Total", y = "y_L_AMY_L_dAI", color = "Sex") +
  theme_minimal()

# --------- Prep variables ----------
# sex is coded as a factor (0 = Male, 1 = Female) and center MASC2
df <- df |>
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Male", "Female")),
    MASC2 = as.numeric(total_MASC2_raw),
    MASC2_c = as.numeric(scale(MASC2, center = TRUE, scale = FALSE))
  )

# --------- Fit OLS with interaction ----------
# DV: FC
# IVs: MASC2_c, sex, and their interaction
fit <- lm(y_L_AMY_L_dAI ~ MASC2_c * sex, data = df, na.action = na.omit)

# (Optional) standardized betas (numeric terms only)
fit_beta <- lm.beta(fit)
summary(fit_beta)

# --------- Model table (gtsummary) ----------
tbl <- tbl_regression(
  fit,
  intercept = TRUE,
  label = list(
    MASC2_c ~ "MASC-2 (centered)",
    sex ~ "Sex [Female vs Male]"
  )
) |>
  add_glance_source_note(
    include = c("r.squared", "adj.r.squared", "statistic", "p.value", "df", "df.residual")
  ) |>
  # optional: relabel the interaction row
  modify_table_body(
    ~ .x |>
      mutate(label = ifelse(term == "MASC2_c:sexFemale",
                            "MASC-2 × Sex (Female vs Male)", label))  )

tbl

# ===== Standard model report (base R) =====
s <- summary(fit)
print(s)

# ----- Interaction: β [95% CI], p -----
co <- s$coefficients
nm <- grep("^MASC2_c:sex", rownames(co), value = TRUE)  # e.g., "MASC2_c:sexFemale"
beta_int <- co[nm, "Estimate"]
p_int    <- co[nm, "Pr(>|t|)"]
ci_int   <- confint(fit)[nm, ]  # 95% CI
cat(sprintf("Interaction (%s): β=%.3f, 95%% CI [%.3f, %.3f], p=%.3f\n",
            sub("MASC2_c:", "MASC-2 × ", nm), beta_int, ci_int[1], ci_int[2], p_int))

# ----- Intercept: β [95% CI], p -----
b0  <- co["(Intercept)", "Estimate"]
p0  <- co["(Intercept)", "Pr(>|t|)"]
ci0 <- confint(fit)["(Intercept)", ]
cat(sprintf("Intercept: β=%.3f, 95%% CI [%.3f, %.3f], p=%.3f\n",
            b0, ci0[1], ci0[2], p0))

# --------- Simple slopes (interaction probe) ----------
# test the slope of MASC2_c within each sex level (i.e., Male vs Female)
ss <- interactions::sim_slopes(fit, pred = MASC2_c, modx = sex, johnson_neyman = FALSE)
ss # prints the simple slopes (estimate, SE, t, p) for Males and Females

# --------- Plot (nice for the poster/manuscript) ----------
interact_plot(
  fit, pred = MASC2_c, modx = sex,
  plot.points = TRUE, interval = TRUE
)

# Wald test: anxiety X sex interaction terms,to see if it improves the model fit.
# --- Fit reduced (no interaction) and full (with interaction) models ---
fit0 <- lm(y_L_AMY_L_dAI ~ MASC2_c + sex, data = df, na.action = na.omit)
fit1 <- lm(y_L_AMY_L_dAI ~ MASC2_c * sex, data = df, na.action = na.omit)
lmtest::waldtest(fit0, fit1)
