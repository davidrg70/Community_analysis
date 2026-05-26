# dg_corr_and_nonlinear_regress_GT_anxiety.R

codes_dir <- "/proj/belgerlab/projects/data/PASS/code/david/"
setwd(codes_dir)

rm(list = ls())

library(readxl)
library(dplyr)
library(stringr)
library(R.matlab)
library(tibble)
library(purrr)
library(ggplot2)
library(tidyr)
library(broom)

gt_data_dir  <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/GT_metrics/Analysis_4_November2025/"
gt_anx_data_file <- "/proj/belgerlab/projects/data/PASS/derivatives/david_work/GT_metrics/Analysis_4_November2025/Rest_GT_sSCT_with_MASC2scores.xlsx"
gt_anx_data <- read_excel(gt_anx_data_file, sheet = "Sheet1")

# ===============================
# GT metrics vs Anxiety: 14 figs
# ===============================

# Assumes you've already run your setup up to:
# gt_anx_data <- read_excel(gt_anx_data_file, sheet = "Sheet1")

# ---- Helpers ----
num_or_na <- function(x) suppressWarnings(as.numeric(x))

# Pick GT metrics and anxiety variables
gt_vars <- c(
  "GE",
  "WD_ControlNetwork", "WD_SalienceNetwork", "WD_ThreatNetwork",
  "PC_ControlNetwork", "PC_SalienceNetwork", "PC_ThreatNetwork"
)

anx_vars <- names(gt_anx_data) |>
  (\(nms) nms[startsWith(nms, "child_")])()

# Coerce to numeric (safely) for all used columns
gt_anx_data <- gt_anx_data |>
  mutate(across(all_of(c(gt_vars, anx_vars)), num_or_na)) |>
  # drop any rows with NA in any of the variables we will use
  drop_na(all_of(c(gt_vars, anx_vars)))

message("N rows after NA-drop: ", nrow(gt_anx_data))

# ------------- SPEARMAN CORRELATIONS (7 figs) -------------
for (x in gt_vars) {
  # Compute rho/p per anxiety var for this GT metric
  cor_tbl <- purrr::map_dfr(anx_vars, function(y) {
    vx <- gt_anx_data[[x]]
    vy <- gt_anx_data[[y]]
    ct <- suppressWarnings(cor.test(vx, vy, method = "spearman", exact = FALSE))
    tibble(
      metric = x,
      anxiety_var = y,
      n = sum(is.finite(vx) & is.finite(vy)),
      rho = unname(ct$estimate),
      p = ct$p.value
    )
  }) |>
    mutate(p_fdr = p.adjust(p, method = "fdr"))
  
  # Prepare per-panel annotation (rho, p, FDR-p)
  # also compute label anchor positions per facet
  panel_ranges <- gt_anx_data |>
    select(all_of(x), all_of(anx_vars)) |>
    pivot_longer(cols = all_of(anx_vars), names_to = "anxiety_var", values_to = "anxiety_value") |>
    group_by(anxiety_var) |>
    summarize(
      x_min = min(.data[[x]], na.rm = TRUE),
      x_max = max(.data[[x]], na.rm = TRUE),
      y_min = min(anxiety_value, na.rm = TRUE),
      y_max = max(anxiety_value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      cor_tbl |>
        mutate(label = paste0(
          "ρ = ", sprintf("%.3f", rho),
          "\np = ", signif(p, 3),
          "\np(FDR) = ", signif(p_fdr, 3)
        )) |>
        select(anxiety_var, label),
      by = "anxiety_var"
    ) |>
    mutate(
      label_x = x_min + 0.02 * (x_max - x_min),
      label_y = y_max - 0.05 * (y_max - y_min)
    )
  
  p_cor <- ggplot(
    gt_anx_data |>
      select(all_of(x), all_of(anx_vars)) |>
      pivot_longer(cols = all_of(anx_vars), names_to = "anxiety_var", values_to = "anxiety_value"),
    aes(x = .data[[x]], y = anxiety_value)
  ) +
    geom_point(color = "#3b7fc4", alpha = 0.8) +
    # "usual linear fitting line" for correlation plots
    stat_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "black") +
    facet_wrap(~ anxiety_var, scales = "free_y", ncol = 3) +
    geom_text(
      data = panel_ranges,
      aes(x = label_x, y = label_y, label = label),
      hjust = 0, vjust = 1, size = 3
    ) +
    labs(
      title = paste0("Spearman correlations: ", x, " vs. anxiety (ρ, p, FDR-p)"),
      x = x, y = "Anxiety score"
    ) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "none")
  
  ggsave(
    filename = file.path(gt_data_dir, paste0("corr_spearman_", x, ".png")),
    plot = p_cor, width = 12, height = 11, dpi = 300
  )
}

# ------------- NONLINEAR REGRESSIONS (7 figs) -------------
# Quadratic model (y ~ x + x^2) + LOESS overlay
fit_quad_summarize <- function(df, xvar, yvar) {
  f  <- as.formula(paste0(yvar, " ~ ", xvar, " + I(", xvar, "^2)"))
  fit <- lm(f, data = df)
  sm  <- summary(fit)
  fsta <- sm$fstatistic
  fval <- unname(fsta["value"]); df1 <- unname(fsta["numdf"]); df2 <- unname(fsta["dendf"])
  pval <- pf(fval, df1, df2, lower.tail = FALSE)
  tibble(
    metric = xvar,
    anxiety_var = yvar,
    n = nobs(fit),
    adj_r2 = sm$adj.r.squared,
    F_value = fval,
    df_num = df1,
    df_den = df2,
    p_value = pval
  )
}

for (x in gt_vars) {
  # Per-anxiety var stats
  quad_stats <- purrr::map_dfr(anx_vars, ~ fit_quad_summarize(gt_anx_data, x, .x))
  
  # Data long for plotting
  df_long <- gt_anx_data |>
    select(all_of(x), all_of(anx_vars)) |>
    pivot_longer(cols = all_of(anx_vars), names_to = "anxiety_var", values_to = "anxiety_value")
  
  # Label positions + text (adj R² + model p)
  panel_ranges <- df_long |>
    group_by(anxiety_var) |>
    summarize(
      x_min = min(.data[[x]], na.rm = TRUE),
      x_max = max(.data[[x]], na.rm = TRUE),
      y_min = min(anxiety_value, na.rm = TRUE),
      y_max = max(anxiety_value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      quad_stats |>
        mutate(label = paste0(
          "adj R² = ", sprintf("%.3f", adj_r2),
          "\nF(", df_num, ",", df_den, ") = ", sprintf("%.2f", F_value),
          "\np = ", signif(p_value, 3)
        )) |>
        select(anxiety_var, label),
      by = "anxiety_var"
    ) |>
    mutate(
      label_x = x_min + 0.02 * (x_max - x_min),
      label_y = y_max - 0.05 * (y_max - y_min)
    )
  
  p_reg <- ggplot(df_long, aes(x = .data[[x]], y = anxiety_value)) +
    geom_point(color = "#7e2551", alpha = 0.8) +
    # Quadratic fit (parametric)
    stat_smooth(
      method = "lm",
      formula = y ~ poly(x, 2, raw = TRUE),
      se = FALSE,
      aes(color = "Quadratic"),
      linewidth = 0.9
    ) +
    # LOESS (nonparametric)
    stat_smooth(
      method = "loess",
      span = 0.8, degree = 2,
      se = FALSE,
      aes(color = "LOESS"),
      linewidth = 0.9
    ) +
    facet_wrap(~ anxiety_var, scales = "free_y", ncol = 3) +
    geom_text(
      data = panel_ranges,
      aes(x = label_x, y = label_y, label = label),
      hjust = 0, vjust = 1, size = 3
    ) +
    scale_color_manual(
      name = "Fit",
      values = c("Quadratic" = "black", "LOESS" = "firebrick")
    ) +
    labs(
      title = paste0("Nonlinear fits: ", x, " predicting anxiety (quadratic + LOESS)"),
      x = x, y = "Anxiety score"
    ) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
  ggsave(
    filename = file.path(gt_data_dir, paste0("nonlinear_quad_loess_", x, ".png")),
    plot = p_reg, width = 12, height = 11, dpi = 300
  )
}

message("Saved 14 figures to: ", gt_data_dir)
