# dg_FC_Anxiety_OLS.R
# OLS model (if age and sex were used as covariates in FC analysis...otherwise, include them in the OLS model?)
#     gPPI R2R FC ~ child_GADIndex_Raw + 
#                   child_SeparationAnxiety_Phobias_Raw + 
#                   child_SocialAnxietyTotal_Raw + 
#                   child_PerformanceFears_Raw + 
#                   child_ObsessionsCompulsions_Raw + 
#                   child_PhysicalSymptomsTotal_Raw + 
#                   child_Panic_Raw + child_HarmAvoidance_Raw + 
#                   RSA_baseline
# ALL INDEPENDENT VARIABLES DEMEANED (MEAN-CENTERED)!
# total_MASC2_raw is not included in the model!! As it collects all the other MASC2 subscores, and to avoid multicollinearity!

rm(list = ls())

# Packages (install if needed)
# install.packages(c("lm.beta","readxl","ggplot2"))
# Optional (uncomment to install if you want VIFs): install.packages("car")

library(lm.beta)
library(readxl)
library(ggplot2)
# library(car)

directory <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/"
file <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/Regressions/Analysis_6_August2025/gPPI4_Data_for_OLS.xlsx"

# --------- Load data ----------
df <- read_excel(file, col_names = TRUE, na = c("", "NA"))

# ---- Define DV + IVs ----
dv <- "HLEmo_gt_LLEmo_LDefaultA_PFCm2_LContA_IPS1"
iv_original <- c(
  "child_GADIndex_Raw",
  "child_SeparationAnxiety_Phobias_Raw",
  "child_SocialAnxietyTotal_Raw",
  "child_PerformanceFears_Raw",
  "child_ObsessionsCompulsions_Raw",
  "child_PhysicalSymptomsTotal_Raw",
  "child_Panic_Raw",
  "child_HarmAvoidance_Raw",
  "RSA_baseline"   # note the space; handled below
)

# Keep only necessary columns (ID optional if present)
keep <- c(dv, iv_original)
if ("ID" %in% names(df)) keep <- c("ID", keep)
stopifnot(all(c(dv, iv_original) %in% names(df))) 

# Convert all but ID to numeric (safe for tibbles)
num_cols <- setdiff(names(df), "ID")
df[num_cols] <- lapply(df[num_cols], function(x) as.numeric(x))

# Drop rows with missing DV
df <- df[!is.na(df[[dv]]), ]

# ---- Mean-center IVs (create _demeaned columns with safe names) ----
make_safe <- function(nm) {
  nm2 <- gsub("[^[:alnum:]]+", "_", nm)  # spaces/punct -> underscores
  nm2 <- gsub("_+", "_", nm2)
  nm2 <- sub("^_", "", nm2)
  nm2 <- sub("_$", "", nm2)
  nm2
}

iv_demeaned <- character(length(iv_original))
for (i in seq_along(iv_original)) {
  src <- iv_original[i]
  safe <- make_safe(src)
  newn <- paste0(safe, "_demeaned")
  iv_demeaned[i] <- newn
  df[[newn]] <- df[[src]] - mean(df[[src]], na.rm = TRUE)
}

# --------- Fit OLS (demeaned IVs; NO age/sex) ----------
form <- as.formula(paste(dv, "~", paste(iv_demeaned, collapse = " + ")))
m1 <- lm(form, data = df, na.action = na.exclude)
s <- summary(m1)

cat("\n=== MODEL FIT (EF DV, demeaned IVs; no age/sex) ===\n")
cat("N used:", nobs(m1), "\n")
cat(sprintf("R-squared: %.4f, Adjusted R-squared: %.4f\n",
            s$r.squared, s$adj.r.squared))
cat(sprintf("F(%d, %d) = %.3f, p = %.4g\n",
            s$fstatistic[2], s$fstatistic[3],
            s$fstatistic[1],
            pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)))

# --------- Coefficients, CI, standardized betas ----------
coef_tab <- data.frame(
  term = rownames(s$coefficients),
  estimate = s$coefficients[, "Estimate"],
  std_error = s$coefficients[, "Std. Error"],
  t_value = s$coefficients[, "t value"],
  p_value = s$coefficients[, "Pr(>|t|)"],
  row.names = NULL, check.names = FALSE
)

ci <- confint(m1, level = 0.95)
ci_tab <- data.frame(
  term = rownames(ci),
  conf_low = ci[, 1],
  conf_high = ci[, 2],
  row.names = NULL, check.names = FALSE
)

std_b <- lm.beta(m1)
std_tab <- data.frame(
  term = names(coef(std_b)),
  std_beta = coef(std_b),
  row.names = NULL, check.names = FALSE
)

out_tab <- Reduce(function(a, b) merge(a, b, by = "term", all.x = TRUE, all.y = FALSE),
                  list(coef_tab, ci_tab, std_tab))

cat("\n=== COEFFICIENTS (demeaned IVs; no age/sex) ===\n")
print(out_tab, row.names = FALSE, digits = 2)

# --------- Diagnostics ----------
p1 <- ggplot(data.frame(fitted = fitted(m1), resid = resid(m1)),
             aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (EF DV, demeaned IVs)", x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 12)

qq_df <- data.frame(sample = resid(m1))
p2 <- ggplot(qq_df, aes(sample = sample)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Normal Q-Q of Residuals (EF DV, demeaned IVs)") +
  theme_minimal(base_size = 12)

print(p1); print(p2)

# --------- Save outputs ----------
if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)

write.csv(out_tab,
          file.path(directory, "OLS_noAge_coefficients_with_CI_stdBeta.csv"),
          row.names = FALSE)

fit_stats <- data.frame(
  n = nobs(m1),
  r_squared = s$r.squared,
  adj_r_squared = s$adj.r.squared,
  f_stat = unname(s$fstatistic[1]),
  df_num = unname(s$fstatistic[2]),
  df_den = unname(s$fstatistic[3]),
  f_p_value = pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
)
write.csv(fit_stats,
          file.path(directory, "OLS_noAge_fit_stats.csv"),
          row.names = FALSE)

cat("\nSaved:\n",
    file.path(directory, "OLS_noAge_coefficients_with_CI_stdBeta.csv"), "\n",
    file.path(directory, "OLS_noAge_fit_stats.csv"), "\n")
