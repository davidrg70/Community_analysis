# dg_LMM_activation_anxiety.R

# ---- Clean environment ----
rm(list = ls())

# ---- Libraries ----
library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)

# ---- Directories ----
codes_dir <- "/proj/belgerlab/projects/data/PASS/code/david/"
data_dir  <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/LLMs/"
setwd(codes_dir)

# ---- Read data ----
efitData1 <- read_xlsx(path = file.path(data_dir, "EFIT_dprime_activation_62p.xlsx"))
efitData1 <- as.data.frame(efitData1)

# ---- Basic cleaning ----
efitData1 <- efitData1 %>%
  mutate(
    Participant_ID = as.character(Participant_ID),
    Sex = as.factor(Sex),
    MASC2_GAD_Tscore = as.numeric(MASC2_GAD_Tscore)
  )

## --- LONG FORMAT FOR ALL DATA
# each participant to have 4 d prime columns
dprime_long <- efitData1 %>%
  select(
    Participant_ID,
    Sex,
    MASC2_GAD_Tscore,
    starts_with("dp_")
  ) %>%
  pivot_longer(
    cols = starts_with("dp_"),
    names_to = "condition",
    values_to = "dprime"
  ) %>%
  mutate(
    condition = gsub("^dp_", "", condition)
  )

# activation values with long format (16 columns per participant)
activation_long <- efitData1 %>%
  select(
    Participant_ID,
    starts_with("HLEmo_"),
    starts_with("HLNeu_"),
    starts_with("LLEmo_"),
    starts_with("LLNeu_")
  ) %>%
  pivot_longer(
    cols = -Participant_ID,
    names_to = c("condition", "ROI"),
    names_pattern = "^(HLEmo|HLNeu|LLEmo|LLNeu)_(.*)$",
    values_to = "activation"
  )

# ---- Join d prime and activation data ----
efit_activation_long <- activation_long %>%
  left_join(
    dprime_long,
    by = c("Participant_ID", "condition")
  )

# ---- Plot data before LMM, but with a LMM-aligned format ----
library(ggplot2)
# Source plotting function
source(file.path(codes_dir, "plot_efit_lmm_activation_data.R"))
# Run plotting function
efit_lmm_plot <- plot_efit_lmm_data(
  data_long = efit_activation_long,
  output_dir = data_dir,
  file_prefix = "EFIT_activation_dprime",
  save_plot = TRUE,
  show_pooled_line = TRUE)
# Display plots in RStudio
efit_lmm_plot$plot

# ---- Create model variables ----
efit_activation_long <- efit_activation_long %>%
  mutate(
    # Rename variables for the model
    subject = Participant_ID,
    sex = Sex,
    anxiety = MASC2_GAD_Tscore,
    
    # Define load
    load = case_when(
      condition %in% c("HLEmo", "HLNeu") ~ "High",
      condition %in% c("LLEmo", "LLNeu") ~ "Low"
    ),
    
    # Define emotion
    emotion = case_when(
      condition %in% c("HLEmo", "LLEmo") ~ "Emotional",
      condition %in% c("HLNeu", "LLNeu") ~ "Neutral"
    ),
    
    # Random-effect grouping variable
    subject_condition = interaction(subject, condition, drop = TRUE)
  )

# ---- Convert categorical variables to factors ----
efit_activation_long <- efit_activation_long %>%
  mutate(
    subject = as.factor(subject),
    sex = as.factor(sex),
    condition = factor(
      condition,
      levels = c("LLNeu", "LLEmo", "HLNeu", "HLEmo")
    ),
    load = factor(
      load,
      levels = c("Low", "High")
    ),
    emotion = factor(
      emotion,
      levels = c("Neutral", "Emotional")
    ),
    ROI = factor(
      ROI,
      levels = c("L_dACC", "R_dACC", "L_dAIns", "R_dAIns")
    ),
    subject_condition = as.factor(subject_condition)
  )

# With this coding:
# Low load is the reference level.
# Neutral is the reference level.
# LLNeu is the reference condition.
# L_dACC is the reference ROI.

# ---- Z-score activation and anxiety ----
efit_activation_long <- efit_activation_long %>%
  mutate(
    activation_z = as.numeric(scale(activation)),
    anxiety_z = as.numeric(scale(anxiety))
  )

# ---- Check the long-format dataset ----
str(efit_activation_long)

dim(efit_activation_long)

table(efit_activation_long$condition)
table(efit_activation_long$ROI)
table(efit_activation_long$load, efit_activation_long$emotion)

head(efit_activation_long)

# ---- Run the activation LMM (subject random-intercept model) with stronger optimizer ----
model_activation_simple <- lmer(
  dprime ~ activation_z * anxiety_z +
    activation_z * load * emotion +
    ROI + sex +
    (1 | subject),
  data = efit_activation_long,
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1000000)
  )
)

summary(model_activation_simple)
anova(model_activation_simple)
isSingular(model_activation_simple, tol = 1e-4)
