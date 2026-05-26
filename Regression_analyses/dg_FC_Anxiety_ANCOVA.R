# dg_FC_Anxiety_ANCOVA.R

# two-way ANCOVA:
# Evaluate the effect of Anxiety and Sex on FC values (optionally: after adjusting for age)

rm(list = ls())

library(readxl)   
library(gtsummary)
library(ggplot2)  
library(dplyr)
library(effectsize)
library(car)

directory <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/"
setwd(directory)
file <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/Analysis_6_August2025/gPPI4_SEED_L_AMY_Data_for_OLS.xlsx"

# --------- Load data ----------
df <- read_excel(file, col_names = TRUE, na = c("", "NA")) |>
  dplyr::rename(y_L_AMY_L_dAI = `y_L_AMY_L-dAI`) |>
  mutate(sex = factor(sex, levels = c(0, 1), labels = c("Male", "Female"))
  )

# scatter plot between the covariate (i.e., anxiety) and the DV (i.e., FC) 
# for each (by) sex as the only "categorical" variable (facets by sex)
ggplot(df, aes(x = total_MASC2_raw, y = y_L_AMY_allSignTargets,
               color = sex)) +
  geom_point(size = 3, alpha = 0.8, na.rm = TRUE) +
  geom_smooth(method = "loess", se = FALSE, span = 0.9, na.rm = TRUE) +
  facet_wrap(~ sex) +
  labs(x = "MASC-2 Total", y = "FC: y_L_AMY_allSignTargets", color = "Sex") +
  theme_minimal()

## load data again...use it without categorical changes to df$sex
df2 <- read_excel(file, col_names = TRUE, na = c("", "NA"))
df2 <- dplyr::rename(df2, y_L_AMY_L_dAI = `y_L_AMY_L-dAI`)

# ANCOVA model with interaction
df2$total_MASC2_raw_c <- scale(df2$total_MASC2_raw, center = TRUE, scale = FALSE) # mean-center total_MASC2_raw
ancova_model <- lm(y_L_AMY_allSignTargets ~ total_MASC2_raw_c * sex, data = df2)

# Run ANOVA/ANCOVA table (F-tests, p-values)
# Use type III sums of squares (standard for ANCOVA when interaction included):
anova_results <- Anova(ancova_model, type = "III")
print(anova_results)

# Extract effect sizes
# Partial eta squared effect sizes
eta_sq_results <- eta_squared(ancova_model, partial = TRUE)
print(eta_sq_results)

# Summarize model, also with gtsummary..
summary(ancova_model)
tbl_regression(ancova_model, exponentiate = FALSE) %>%
  add_glance_table(include = c(r.squared, adj.r.squared, AIC, BIC))
