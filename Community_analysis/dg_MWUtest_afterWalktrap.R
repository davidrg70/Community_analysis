### dg_MWUtest_afterWalktrap ###
rm(list = ls())
library(igraph)

# STEP 1 Load Participants data!
directory <- "/users/d/g/dga/BrainMAP/"
setwd(directory)

# Bring diagnostic data
source("dg_determine_groups.R")
# Bring cleaned data, downloaded from RedCap
source("dg_trim_clinicaldata.R") # get trimmed data and subjects IDs in groups

# STEP 2 Bring clusters - Walktrap results (either ADHD only or ADHD+TD)
# brings modularity (Group of ones, higher item values -BAD ER- || Group of twos, lower item values -GOOD ER-)
selection <- menu(c("ADHD only", "ADHD and TD"), title="Choose groups to do Walktrap clustering analysis")

if(selection == 1){
print("Participants with ADHD selected. ")
gml_file_path <- "/users/d/g/dga/BrainMAP/net_allfeatures_ADHDonly.gml"
load("clustersdata_ADHDonly.RData")
} else if(selection == 2) {
print("Participants with ADHD and also controls with TD selected. ")
gml_file_path <- "/users/d/g/dga/BrainMAP/net_allfeatures_ADHDandTD.gml"
load("clustersdata_ADHDandTD.RData")
}

membership(cluster_allsubs)
modularity(cluster_allsubs)

# STEP 3 Compare clusters in clinical data
cluster_names <- names(membership(cluster_allsubs))
clean_cluster_names <- sub("^a", "", cluster_names) # removes the a label

if (selection == 2){
  clean_cluster_names <- sub("^c", "", cluster_names) # removes the c label, if ADHD+TD analysis
}

cluster_allsubs$names <- clean_cluster_names # rename names in clusters frame

# Compare clusters, using BRIEF subscores!
common_subjects_brief <- intersect(cluster_allsubs$names, data_brief$Subject.ID)
group_1 <- common_subjects_brief[cluster_allsubs$membership == 1] # Separate subjects into two groups based on their membership (1 or 2)
group_2 <- common_subjects_brief[cluster_allsubs$membership == 2]

columns_to_compare_brief <- colnames(data_brief)[colnames(data_brief) != "Subject.ID"] # get all column names in data_brief, excluding 'Subject.ID'
test_results_brief <- list() # initialize
group_1_data_brief <- list()
group_2_data_brief <- list()

for (col in columns_to_compare_brief) {
  scores_group_1 <- data_brief[[col]][data_brief$Subject.ID %in% group_1]
  scores_group_2 <- data_brief[[col]][data_brief$Subject.ID %in% group_2]
  group_1_data_brief[[col]] <- scores_group_1
  group_2_data_brief[[col]] <- scores_group_2
  # Wilcoxon rank-sum test \\ exact=FALSE added to make the test not to assume repeated values (ties)
  test_result <- wilcox.test(scores_group_1, scores_group_2, exact = FALSE) 
  test_results_brief[[col]] <- test_result
}

# Compare clusters, using CBCL subscores!
common_subjects_cbcl <- intersect(cluster_allsubs$names, data_cbcl$Subject.ID)
group_1 <- common_subjects_cbcl[cluster_allsubs$membership == 1] # Separate subjects into two groups based on their membership (1 or 2)
group_2 <- common_subjects_cbcl[cluster_allsubs$membership == 2]
columns_to_compare_cbcl <- colnames(data_cbcl)[colnames(data_cbcl) != "Subject.ID"] # get all column names in data_brief, excluding 'Subject.ID'
test_results_cbcl <- list() # initialize
group_1_data_cbcl <- list()
group_2_data_cbcl <- list()

for (col in columns_to_compare_cbcl) {
  scores_group_1 <- data_cbcl[[col]][data_cbcl$Subject.ID %in% group_1]
  scores_group_2 <- data_cbcl[[col]][data_cbcl$Subject.ID %in% group_2]
  group_1_data_cbcl[[col]] <- scores_group_1
  group_2_data_cbcl[[col]] <- scores_group_2
  # Wilcoxon rank-sum test \\ exact=FALSE added to make the test not to assume repeated values (ties)
  test_result <- wilcox.test(scores_group_1, scores_group_2, exact = FALSE) 
  test_results_cbcl[[col]] <- test_result
}

# Compare clusters, using the only Columbia IS score!
common_subjects_columbia <- intersect(cluster_allsubs$names, data_columbia$Subject.ID)
group_1 <- common_subjects_columbia[cluster_allsubs$membership == 1] # Separate subjects into two groups based on their membership (1 or 2)
group_2 <- common_subjects_columbia[cluster_allsubs$membership == 2]
columns_to_compare_columbia <- colnames(data_columbia)[colnames(data_columbia) != "Subject.ID"] # get all column names in data_brief, excluding 'Subject.ID'
test_results_columbia <- list() # initialize
group_1_data_columbia <- list()
group_2_data_columbia <- list()

for (col in columns_to_compare_columbia) {
  scores_group_1 <- data_columbia[[col]][data_columbia$Subject.ID %in% group_1]
  scores_group_2 <- data_columbia[[col]][data_columbia$Subject.ID %in% group_2]
  group_1_data_columbia[[col]] <- scores_group_1
  group_2_data_columbia[[col]] <- scores_group_2
  # Wilcoxon rank-sum test \\ exact=FALSE added to make the test not to assume repeated values (ties)
  test_result <- wilcox.test(scores_group_1, scores_group_2, exact = FALSE) 
  test_results_columbia[[col]] <- test_result
}

# Apply FDR or Bonferroni correction across ALL subscores analyzed (BRIEF, CBCL, ColumbiaIS)
p_values_brief <- sapply(test_results_brief, function(x) x$p.value)
p_values_cbcl <- sapply(test_results_cbcl, function(x) x$p.value)
p_values_columbia <- sapply(test_results_columbia, function(x) x$p.value)
combined_p_values <- c(p_values_brief, p_values_cbcl, p_values_columbia)
# p_vals_fdr <- p.adjust(combined_p_values, method = "fdr")
p_vals_bnf <- p.adjust(combined_p_values, method = "bonferroni")

# Calculate the medians per group and survey
medians_group1_brief <- lapply(group_1_data_brief, median, na.rm = TRUE)
medians_group2_brief <- lapply(group_2_data_brief, median, na.rm = TRUE)
medians_group1_cbcl <- lapply(group_1_data_cbcl, median, na.rm = TRUE)
medians_group2_cbcl <- lapply(group_2_data_cbcl, median, na.rm = TRUE)
medians_group1_columbia <- lapply(group_1_data_columbia, median, na.rm = TRUE)
medians_group2_columbia <- lapply(group_2_data_columbia, median, na.rm = TRUE)

# Lastly, get the W statistic of every survey
w_stats_brief <- sapply(test_results_brief, function(x) x$statistic)
w_stats_cbcl <- sapply(test_results_cbcl, function(x) x$statistic)
w_stats_columbia <- sapply(test_results_columbia, function(x) x$statistic)

## STEP 4 Plot with violin plots to check for data distribution --- OPTIONAL, but nice :)
## NOTE: I preferred a "zero-padding" approach instead of truncating the longer vector to match the length of the shorter vector
## The second removes many data points and makes the violin plots highly inaccurate! The first pads with NA the shorter vector...
library(ggplot2)

# BRIEF - Inhibition Raw Score
# Pad the shorter cluster with NA values to make both groups the same length
max_length <- max(length(group_1_data_brief$Inhibition.Raw.Score), 
                  length(group_2_data_brief$Inhibition.Raw.Score))
group_1_padded <- c(group_1_data_brief$Inhibition.Raw.Score, 
                    rep(NA, max_length - length(group_1_data_brief$Inhibition.Raw.Score)))
group_2_padded <- c(group_2_data_brief$Inhibition.Raw.Score, 
                    rep(NA, max_length - length(group_2_data_brief$Inhibition.Raw.Score)))
combined_data <- data.frame( # Combine the padded data
  Score = c(group_1_padded, group_2_padded),
  Group = factor(rep(c("Cluster High ER difficulties", "Cluster Low ER difficulties"), each = max_length))
)
# now plot!
ggplot(combined_data, aes(x = Group, y = Score)) +
  geom_violin(trim = FALSE, fill = "lightblue", color = "black") + # Violin plot
  geom_jitter(width = 0.1, size = 1.5, color = "darkblue", alpha = 0.6) + # Jitter for individual points (dots)
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "green") + # Median points
  labs(title = "BRIEF -Inhibition Raw Score Comparison", x = "Cluster", y = "Inhibition Raw Score") + # Axis labels and title
  theme_minimal() + # Clean theme
  scale_y_continuous(limits = c(min(combined_data$Score) - 1, max(combined_data$Score) + 1)) # Control y-axis limits

# CBCL - Attention Problems Syndrome Subscale
# Pad the shorter cluster with NA values to make both groups the same length
max_length <- max(length(group_1_data_cbcl$Attention.Problems.Syndrome.Subscale), 
                  length(group_2_data_cbcl$Attention.Problems.Syndrome.Subscale))
group_1_padded <- c(group_1_data_cbcl$Attention.Problems.Syndrome.Subscale, 
                    rep(NA, max_length - length(group_1_data_cbcl$Attention.Problems.Syndrome.Subscale)))
group_2_padded <- c(group_2_data_cbcl$Attention.Problems.Syndrome.Subscale, 
                    rep(NA, max_length - length(group_2_data_cbcl$Attention.Problems.Syndrome.Subscale)))
combined_data <- data.frame( # Combine the padded data
  Score = c(group_1_padded, group_2_padded),
  Group = factor(rep(c("Cluster High ER difficulties", "Cluster Low ER difficulties"), each = max_length))
)
# now plot!
ggplot(combined_data, aes(x = Group, y = Score)) +
  geom_violin(trim = FALSE, fill = "lightblue", color = "black") + # Violin plot
  geom_jitter(width = 0.1, size = 1.5, color = "darkblue", alpha = 0.6) + # Jitter for individual points (dots)
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "green") + # Median points
  labs(title = "CBCL - Attention Problems Syndrome Subscale Comparison", x = "Cluster", y = "Attention Problems Syndrome Subscale Score") + # Axis labels and title
  theme_minimal() + # Clean theme
  scale_y_continuous(limits = c(min(combined_data$Score) - 1, max(combined_data$Score) + 1)) # Control y-axis limits

# CBCL - Aggressive Behavior Syndrome Subscale
# Pad the shorter cluster with NA values to make both groups the same length
max_length <- max(length(group_1_data_cbcl$Aggressive.Behavior.Syndrome.Subscale), 
                  length(group_2_data_cbcl$Aggressive.Behavior.Syndrome.Subscale))
group_1_padded <- c(group_1_data_cbcl$Aggressive.Behavior.Syndrome.Subscale, 
                    rep(NA, max_length - length(group_1_data_cbcl$Aggressive.Behavior.Syndrome.Subscale)))
group_2_padded <- c(group_2_data_cbcl$Aggressive.Behavior.Syndrome.Subscale, 
                    rep(NA, max_length - length(group_2_data_cbcl$Aggressive.Behavior.Syndrome.Subscale)))
combined_data <- data.frame( # Combine the padded data
  Score = c(group_1_padded, group_2_padded),
  Group = factor(rep(c("Cluster High ER difficulties", "Cluster Low ER difficulties"), each = max_length))
)
# now plot!
ggplot(combined_data, aes(x = Group, y = Score)) +
  geom_violin(trim = FALSE, fill = "lightblue", color = "black") + # Violin plot
  geom_jitter(width = 0.1, size = 1.5, color = "darkblue", alpha = 0.6) + # Jitter for individual points (dots)
  stat_summary(fun = "median", geom = "point", shape = 23, size = 3, fill = "green") + # Median points
  labs(title = "CBCL - Aggressive Behavior Syndrome Subscale Comparison", x = "Cluster", y = "Aggressive Behavior Syndrome Subscale Score") + # Axis labels and title
  theme_minimal() + # Clean theme
  scale_y_continuous(limits = c(min(combined_data$Score) - 1, max(combined_data$Score) + 1)) # Control y-axis limits
