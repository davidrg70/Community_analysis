### dg_itemlevel_data_converter_EF_v2 ###
# David Garnica, UNC, September 2024, Updated March 2025, April 2026

# NOTE: RUN FIRST THE "dg_trim_EF_data_v4.m" OR "dg_trim_EF_data_v5.m" SCRIPTS!!!

# install.packages("tidyverse")
# install.packages("e1071") 
rm(list = ls())

# Bring data from excel sheet
directory <- "/users/d/g/dga/BrainMAP/EF_data3/"
setwd(directory)
# SINCE NOV 2025, RAW SCORES, NO SCORES CONVERTED TO INTEGERS!

# WAVE 1
data <- readxl::read_xlsx("SingleEF_W1_table_filtered_2026_04_13.xlsx", sheet = "Sheet1", range="A1:Q126")
data_original <- readxl::read_xlsx("SingleEF_W1_table_filtered_2026_04_13.xlsx", sheet = "Sheet1", range="A1:Q126")

# WAVE 2
data <- readxl::read_xlsx("SingleEF_W2_table_filtered_2026_04_13.xlsx", sheet = "Sheet1", range="A1:M56")
data_original <- readxl::read_xlsx("SingleEF_W2_table_filtered_2026_04_13.xlsx", sheet = "Sheet1", range="A1:M56")

directory <- "/users/d/g/dga/BrainMAP/EF_data3/EF_clustering/"
setwd(directory)
library(dplyr)
library(tidyr)
library(tibble)

# Trim table/data --------------------------------------------------------------------------------------------------------------------------------------
# First, determine all columns names and, thus, ef tasks/tests
ef_tests <- colnames(data) # extract column/tasks names from the dataset
# rename DCCST and Flanker variables
ef_tests <- dplyr::recode(
  ef_tests,
  DCCST_card_rt_shift_cost        = "DCCST_rt_shift_cost",
  DCCST_card_acc_shift_cost       = "DCCST_acc_shift_cost",
  Flanker_flanker_acc_incog_cost  = "Flanker_acc_incog_cost",
  Flanker_flanker_rt_incog_cost   = "Flanker_rt_incog_cost")

subject_ids <- data$subject_id
data <- data[, -1] # remove the first column (of subject ids)
# Rescale data -----------------------------------------------------------------------------------------------------------------------------------------
# A LEAST COMMON MULTIPLE (LCM) APPROACH IS NO LONGER EFFECTIVE, AS IT WORKS FOR 
# ORDINAL DATA BUT IT IS NOT IDEAL FOR CONTINUOUS OR MIXED-RANGE DATA..........
# library(numbers)
# data_lcm <- mLCM(c(unique_numbers))

# THUS, I USE Z-SCORE STANDARIZATION TO RE-SCALE THE CONTINUOUS DATA
# this is using RStudio scale function, and no other packages!
# scale(data_frame$variable, center = TRUE, scale = TRUE)
# Subtracts the mean (centering) and Divides by standard deviation (scaling)
# Transforms values into Z-scores. The new mean is 0 and the new standard deviation is 1.
rescaled_data <- as.data.frame(lapply(data, function(x) scale(x, center = TRUE, scale = TRUE)))
ef_tests_no_id <- ef_tests[ef_tests != "subject_id"] # give columns the correct name, except IDs
stopifnot(length(ef_tests_no_id) == ncol(rescaled_data))
colnames(rescaled_data) <- ef_tests_no_id
unique_numbers <- sort(unique(unlist(data)))
nonzero_numbers <- unique_numbers[unique_numbers != 0] # remove 0 values ...

# summary(rescaled_data) # this allows me to check the data distribution!
# get SUMMARY in a better way I can copy+paste easily in Excel! ---------------------------------------------------
summary_rescaled_data <- rescaled_data %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N      = sum(!is.na(Value)),
    Median = median(Value, na.rm = TRUE),
    Q1     = quantile(Value, 0.25, na.rm = TRUE),
    Q3     = quantile(Value, 0.75, na.rm = TRUE),
    IQR    = IQR(Value, na.rm = TRUE),
    SD     = sd(Value, na.rm = TRUE),
    Min    = min(Value, na.rm = TRUE),
    Max    = max(Value, na.rm = TRUE),
    .groups = "drop"  )
summary_rescaled_data

# Plot to check the ~ normal distribution ---------------------------------------------------
# as it is now, the code makes a plot per every EF test/task
library(ggplot2)
library(e1071)

rescaled_long <- rescaled_data %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") # Convert to long format for ggplot

# make a skewness table
skew_tbl <- rescaled_long %>%
  group_by(variable) %>%
  summarise(
    skewness = skewness(value, na.rm = TRUE),
    .groups = "drop"  )
# merge skewness back into the long dataframe
rescaled_long <- rescaled_long %>%
  left_join(skew_tbl, by = "variable")

ggplot(rescaled_long, aes(x = value)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    alpha = 0.6,
    color = NA
  ) +
  facet_wrap(~ variable, scales = "free_x") +
  geom_text(
    data = skew_tbl,
    aes(
      x = Inf,
      y = Inf,
      label = paste0("Skew = ", round(skewness, 2))
    ),
    hjust = 1.1,
    vjust = 1.3,
    size = 3.5,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Distribution of Rescaled Data",
    x = "Rescaled score",
    y = "Count"
  ) +
  theme_bw()
# Skewness is about asymmetry of the tails relative to the center
# Skewness ≈ 0 → symmetric distribution
# Skewness > 0 → right-skewed (long right tail)
# Skewness < 0 → left-skewed (long left tail)

# Check correlations between variables ------------ Pearson correlation matrix------------
# 1. Define variables by cluster
inhibition_vars <- c(
  "gng_cv_rt",
  "gng_commission",
  "sst_cvrt_correctGo",
  "sst_SSRT",
  "Flanker_rt_incog_cost")
shifting_vars <- c(
  "shifting_ShiftCostRT",
  "DCCST_rt_shift_cost")
updating_vars <- c(
  "wisc_workmem_cscore",
  grep("vswm|phwm", colnames(rescaled_data), value = TRUE))
# Final order
ordered_vars <- c(inhibition_vars, shifting_vars, updating_vars)
# 2. Reorder dataframe columns
rescaled_data_ord <- rescaled_data[, ordered_vars]

# 3. Correlation matrix
cor_mat <- cor(
  rescaled_data_ord,
  method = "pearson",
  use = "pairwise.complete.obs"
)

# Inspect matrix
round(cor_mat, 2)
# 4. Convert correlation matrix to long format
cor_long <- as.data.frame(cor_mat) %>%
  rownames_to_column("Var1") %>%
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "r"
  )
# Keep the requested order on axes
cor_long$Var1 <- factor(cor_long$Var1, levels = ordered_vars)
cor_long$Var2 <- factor(cor_long$Var2, levels = rev(ordered_vars))

# 5. Cluster sizes and separator positions
n_inhib <- length(inhibition_vars)
n_shift <- length(shifting_vars)
n_updat <- length(updating_vars)
n_total <- length(ordered_vars)
# Vertical separator lines
x_sep1 <- n_inhib + 0.5
x_sep2 <- n_inhib + n_shift + 0.5
# Horizontal separator lines
y_sep1 <- n_total - n_inhib + 0.5
y_sep2 <- n_total - (n_inhib + n_shift) + 0.5

# 6. Cluster label positions
x_centers <- c(
  mean(1:n_inhib),
  mean((n_inhib + 1):(n_inhib + n_shift)),
  mean((n_inhib + n_shift + 1):n_total)
)
y_centers <- c(
  n_total - mean(1:n_inhib) + 1,
  n_total - mean((n_inhib + 1):(n_inhib + n_shift)) + 1,
  n_total - mean((n_inhib + n_shift + 1):n_total) + 1
)
cluster_names <- c("Inhibition", "Shifting", "Updating")

# 7. Plot heatmap
ggplot(cor_long, aes(x = Var1, y = Var2, fill = r)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson r"
  ) +
  geom_vline(xintercept = c(x_sep1, x_sep2), linewidth = 1) +
  geom_hline(yintercept = c(y_sep1, y_sep2), linewidth = 1) +
  coord_fixed(clip = "off") +
  labs(title = "Pearson Correlation Matrix by EF Cluster") +
  theme_bw() +
  theme(
    # remove panel square
    panel.border = element_blank(),
    
    # keep axes and ticks
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    
    axis.title = element_blank(),
    panel.grid = element_blank(),
    
    # move variable labels away from matrix
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      margin = margin(t = 8)
    ),
    axis.text.y = element_text(
      margin = margin(r = 8)
    ),
    
    # margins so cluster labels fit better
    plot.margin = margin(30, 40, 95, 95)
  ) +
  annotate(
    "text",
    x = x_centers,
    y = n_total + 1.9,
    label = cluster_names,
    fontface = "bold",
    size = 2.6)

# ADD IDs BEFORE SAVING A FILE WITH THE DATA ---------------------------------------------------
rescaled_data$subject_id <- subject_ids # add subject_ids to the dataframe
rescaled_data <- rescaled_data[, c(ncol(rescaled_data), 1:(ncol(rescaled_data) - 1))] # move 'subject_ids' to the first column
colnames(rescaled_data)[1] <- "subject_id" # rename the column to 'subject_id' (readable to dg_determine_groups.R)

# Lastly, save the re-scaled data with date and RStudio format :)
current_date <- Sys.Date()
#  CHOOSE FILENAME TO SAVE HERE:
file_name1 <- paste0("Rescaled_EF_data_", current_date, ".rds")
saveRDS(rescaled_data, file = file_name1)
file_name2 <- paste0("Rescaled_EF_data_", current_date, ".csv")
write.csv(rescaled_data, file_name2, row.names = FALSE)
cat("Re-scaled data saved as: ", file_name1, "\n")
cat("Re-scaled data saved as: ", file_name2, "\n")
cat("Files saved at: ", directory, "\n")
