# dg_ATT_FC_Anxiety_bivRegress
# install.packages("lm.beta")
# install.packages("readxl")
# rm(list = ls())
library(lm.beta)
library(readxl)
library(ggplot2)

directory = "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/"
file1 <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/Analysis_4_Mar2025/HighLoad-LowLoad-sgacc_l_ains.csv"
file2 <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/Analysis_4_Mar2025/HighLoadFearAndMadFaces-LowLoadFearAndMadFaces-sgacc_l_ains.csv"
file3 <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/Analysis_4_Mar2025/HighLoadFearAndMadFaces-LowLoadFearAndMadFaces-l_racc_r_dacc.csv"
file4 <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/Analysis_4_Mar2025/HighLoadNeutralFaces-LowLoadNeutralFaces-l_racc_r_dacc.csv"
setwd(directory)
output_folder <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions"

sign_att_data <- read.csv(file1)
att_data1 <- as.data.frame(sign_att_data) # converts the table into a data frame
rm(sign_att_data)

sign_att_data <- read.csv(file2)
att_data2 <- as.data.frame(sign_att_data) # converts the table into a data frame
rm(sign_att_data)

sign_att_data <- read.csv(file3)
att_data3 <- as.data.frame(sign_att_data) # converts the table into a data frame
rm(sign_att_data)

sign_att_data <- read.csv(file4)
att_data4 <- as.data.frame(sign_att_data) # converts the table into a data frame
rm(sign_att_data)

# sgacc and l a ins_BothHighLoad & covariates
model_a <- lm(High.Load.sgacc.and.l.ains ~ MASC2_Tscores, data = att_data1)
model_a_std <- lm.beta(model_a)
summary(model_a_std)
# sgacc and l a ins_BothLowLoad & covariates
model_b <- lm(Low.Load.sgacc.and.l.ains ~ MASC2_Tscores, data = att_data1)
model_b_std <- lm.beta(model_b)
summary(model_b_std)

# sgacc and l a ins_HighLoadFearAndMadFaces & covariates
model_c <- lm(High.Load.Fear.And.Mad.Faces.sgacc.and.l.ains ~ MASC2_Tscores, data = att_data2)
model_c_std <- lm.beta(model_c)
summary(model_c_std)
# sgacc and l a ins_LowLoadFearAndMadFaces & covariates
model_d <- lm(Low.Load.Fear.And.Mad.Faces.sgacc.and.l.ains ~ MASC2_Tscores, data = att_data2)
model_d_std <- lm.beta(model_d)
summary(model_d_std)

# l racc and r dacc_HighLoadFearAndMadFaces & covariates
model_e <- lm(High.Load.Fear.And.Mad.Faces.l.racc.and.r.dacc ~ MASC2_Tscores, data = att_data3)
model_e_std <- lm.beta(model_e)
summary(model_e_std)
# l racc and r dacc_LowLoadFearAndMadFaces & covariates
model_f <- lm(Low.Load.Fear.And.Mad.Faces.l.racc.and.r.dacc ~ MASC2_Tscores, data = att_data3)
model_f_std <- lm.beta(model_f)
summary(model_f_std)

# l racc and r dacc_HighLoadNeutralFaces & covariates
model_g <- lm(High.Load.Neutral.Faces.l.racc.and.r.dacc ~ MASC2_Tscores, data = att_data4)
model_g_std <- lm.beta(model_g)
summary(model_g_std)
# l racc and r dacc_LowLoadNeutralFaces & covariates
model_h <- lm(Low.Load.Neutral.Faces.l.racc.and.r.dacc ~ MASC2_Tscores, data = att_data4)
model_h_std <- lm.beta(model_h)
summary(model_h_std)

## FDR CORRECTION - APPLIED OVER THE NUMBER OF REGRESSIONS PERFORMED
get_main_p_value <- function(model) {
  fstat <- summary(model)$fstatistic
  p_value <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) # calculates p-value from the F-statistic
  return(p_value)
}

models <- list(model_a_std,model_b_std,model_c_std,model_d_std)
pvals_multregress <- sapply(models, get_main_p_value)
# calculate fdr for multiple linear regressions
pvals_multregress_fdr <- p.adjust(pvals_multregress, method = "fdr")

## SOME PLOTTING
# Plot of model A
# Convert Sex to a factor for labeling
att_data1$Sex <- factor(att_data1$Sex, levels = c(0, 1), labels = c("Male", "Female"))
# adding a regression line per group
ggplot(att_data1, aes(x = High.Load.sgacc.and.l.ains, y = MASC2_Tscores, color = Sex, shape = Sex)) +
  geom_point(size = 3.5, alpha = 0.86) +
  geom_smooth(method = "lm", se = TRUE, linetype = "longdash", size = 0.6, alpha = 0.125) +
  labs(
    title = "High Load, Emotional and Neutral Faces (SGACC & Left AIns)",
    x = "Fisher z-transformed rho",
    y = "MASC2 T-scores"
  ) +
  scale_color_manual(values = c("royalblue", "darkorange")) +
  scale_shape_manual(values = c(16, 17)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Plot of model H
# Convert Sex to a factor for labeling
att_data4$Sex <- factor(att_data4$Sex, levels = c(0, 1), labels = c("Male", "Female"))
# adding a regression line per group
ggplot(att_data4, aes(x = Low.Load.Neutral.Faces.l.racc.and.r.dacc, y = MASC2_Tscores, color = Sex, shape = Sex)) +
  geom_point(size = 3.5, alpha = 0.86) +
  geom_smooth(method = "lm", se = TRUE, linetype = "longdash", size = 0.6, alpha = 0.125) +
  labs(
    title = "Low Load, Neutral Faces (Left RACC & Right DACC)",
    x = "Fisher z-transformed rho",
    y = "MASC2 T-scores"
  ) +
  scale_color_manual(values = c("royalblue", "darkorange")) +
  scale_shape_manual(values = c(16, 17)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
