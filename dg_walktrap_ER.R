### dg_Walktrap_ER ###
# David Garnica, UNC, September 2024, Updated Feb 2025, April 2026
# Run Walktrap community analysis. Partially based on Mackenzie's Walktrap_ExampleCode_MEM.R

### dg_Walktrap_EF ###
# David Garnica, UNC, February 2025
# Run Walktrap community analysis. Based on Mackenzie's Walktrap_ExampleCode_MEM.R
rm(list = ls())
library(igraph)     # install.packages("igraph")
library(rstudioapi) # install.packages("rstudioapi")
library(corrplot)
library(R.matlab)

# STEP 1  GET THE REDCAP TRIMMED AND RESCALED DATA
directory <- "/users/d/g/dga/BrainMAP/EF_data/EF_clustering/"
setwd(directory)
source("dg_determine_groups.R") # get trimmed data and subjects IDs in groups
rm(ids_data)
# in the "data" frame all columns are numeric type already, except subject_id that is character

# CHOOSE GROUPS TO RUN WALKTRAP!
print("DG script for Walktrap clustering analysis, Executive Functions data")
selection <- menu(c("ADHD only", "ADHD and TD"), title="Choose groups to do Walktrap clustering analysis")

if(selection == 1){
  print("Participants with ADHD selected. ")
  data <- data[!data$subject_id %in% td_sub_ids, ] # remove TDs from data frame
  data_ids <- data[ ,1]
  # specify ADHDs with an "a" label
  ADHDs <- as.character(ADHDs)        # convert to character type/class
  data_ids <- t(data_ids)             # transpose data_ids and make it char type
  # data_ids <- as.character(data_ids)  # convert to character type/class, temporarily
  common_elements <- data_ids %in% ADHDs
  data_ids[common_elements] <- paste0("a", data_ids[common_elements]) # prepend "a" label
} else if(selection == 2) {
  print("Participants with ADHD and also controls/TD selected. ")
  data_ids <- data[ ,1]
  # specify ADHDs with an "a" label
  ADHDs <- as.character(ADHDs)        # convert to character type/class
  data_ids <- as.character(data_ids)  # convert to character type/class, temporarily
  common_elements <- data_ids %in% ADHDs
  data_ids[common_elements] <- paste0("a", data_ids[common_elements]) # prepend "a" label
  # specify TDs with a "c" label (for "controls")
  TDs <- as.character(TDs)
  common_elements <- data_ids %in% TDs
  data_ids[common_elements] <- paste0("c", data_ids[common_elements]) # prepend "a" label
} else {
  stop("Input has to be 1 or 2. Please, repeat. Script stopped.")
}

# STEP 2 Transpose data frame - We want to cluster subjects (not variables), so we need to transpose the correlation matrix such that each subject has a column.
# transpose data frame
data <- as.data.frame(t(data), stringsAsFactors = FALSE)
# remove subjects id row
data <- data[-1,]

# reconvert data_ids to frame type/class
data_ids <- data.frame(data_ids)
# transpose data_ids frame if both ADHD and TD were selected
if(selection == 2){
  data_ids <- as.data.frame(t(data_ids), stringsAsFactors = FALSE)
}

# bring IDs as the columns names!
colnames(data) <- data_ids
# NOTE: in this way it will be possible to know in the plot who was clinically defined as ADHD or TD
#       and we can check how participants are mixed in the resultant clusters

# convert all data elements numeric, as after transposition they are character type!
data[] <- lapply(data, function(x) as.numeric(as.character(x)))

# STEP 3 Create a correlation matrix from the timecourse data
corr_matrix <- as.data.frame(cor(data, method = 'pearson', use = 'complete.obs'))
corr_matrix_allsubs <- as.matrix(corr_matrix)

# STEP 4 Run Community Detection on all subs
# H1.1. thresholding for Walktrap: look at quantiles to decide how much to cut out
hist(corr_matrix_allsubs) # check correlations tendencies, ie. if follow a kind-of normal distribution

# H1.2. --OPTIONAL-- Plot correlation matrix
corrplot(corr_matrix_allsubs, method = "color", col = colorRampPalette(c("blue", "white", "red"))(200),
         type = "lower", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.5)

# H2. remove negative correlations + threshold (for Walktrap)
corr_matrix_allsubs[which(corr_matrix_allsubs < 0.2, arr.ind = T)] = 0

# H3. transform correlation matrix into igraph object
net_allsubs <-  graph_from_adjacency_matrix(corr_matrix_allsubs, mode = "undirected", diag = F, weighted = T)

#### NOTES FROM TEAGUE ON USING igraph
# NOTE: very important that you specify that diag = FALSE (the autocorrelation); the default in igraph is diag = TRUE *DUMB* 
#       (this allows for self edges, which is allowed in graph theory but is meaningless for our purposes); a lot of the algorithms 
#       will use this information and spit out meaningless results
# NOTE: always want weighted = TRUE because igraph has strange behavior when it comes to weights; correlations are below one and 
#       will binarize them and make weird networks
# NOTE: "igraph is a dangerous viper; do not be complacent; the defaults are often incorrect" - TRH

# David's addition - Modularity maximization and Optimal number of steps
# Track the modularity score for different values of steps. The optimal number of steps would generally
# correspond to the highest modularity score. Get/calculate the best number of steps to use later!
modularity_scores <- sapply(1:30, function(step) {
  cluster <- cluster_walktrap(net_allsubs, weights = E(net_allsubs)$weight, steps = step)
  return(modularity(cluster))
})
optimal_steps <- which.max(modularity_scores)

# H4. run Walktrap to conduct clustering
cluster_allsubs <- cluster_walktrap(net_allsubs, weights = E(net_allsubs)$weight, steps = optimal_steps, merges = FALSE, modularity = TRUE, membership = TRUE)

# get information from the Walktrap output
modularity(cluster_allsubs) # get the modularity of the Walktrap-derived partition
membership(cluster_allsubs) # get membership of every subject/participant

# Display-only labels (do not change V(...)$name)
labs <- V(net_allsubs)$name
display_labels <- ifelse(grepl("^a", labs), "ADHD",
                  ifelse(grepl("^c", labs), "TD", labs))
plot(cluster_allsubs, net_allsubs,
     vertex.label       = display_labels,
     vertex.label.cex   = 0.7,   # smaller label font
     vertex.size        = 16,    # optional: adjust node size
     vertex.label.color = "black")

# number of clusters!
unique_clusters <- length(sort(unique(cluster_allsubs$membership)))

## PLOT WITH GGNET2 (From GGally)
library(igraph)
library(intergraph)
library(network)
library(GGally)
library(sna)
library(ggplot2)
# 1. Start with existing igraph object
#   net_allsubs
#   cluster_allsubs
# 2. Add node-level attributes to the igraph object
#    Use character/numeric, not factor
V(net_allsubs)$walktrap_cluster <- as.character(membership(cluster_allsubs))
V(net_allsubs)$diagnosis <- ifelse(
  grepl("^a", V(net_allsubs)$name), "ADHD",
  ifelse(grepl("^c", V(net_allsubs)$name), "TD", "Other")
)
V(net_allsubs)$strength <- as.numeric(
  strength(net_allsubs, weights = E(net_allsubs)$weight)
)
V(net_allsubs)$display_label <- as.character(V(net_allsubs)$name)
# Optional: check attributes before conversion
str(vertex_attr(net_allsubs))
# 3. Convert igraph -> network
net_plot <- intergraph::asNetwork(net_allsubs)
# 4. Layout
set.seed(123)
xy <- sna::gplot.layout.fruchtermanreingold(net_plot, NULL)
# determine how many communities were found
cluster_ids <- sort(unique(as.character(membership(cluster_allsubs))))
n_clusters <- length(cluster_ids)
# define up to 6 cluster colors manually
cluster_palette_master <- c(
  "1" = "#E76F51",
  "2" = "#2A9D8F",
  "3" = "#3A86FF",
  "4" = "#8338EC",
  "5" = "#FFBE0B",
  "6" = "#FB5607"
)
# stop if more than 6 communities are found
if (n_clusters > length(cluster_palette_master)) {
  stop(paste(
    "The current palette supports up to",
    length(cluster_palette_master),
    "communities, but",
    n_clusters,
    "were found. Add more colors to cluster_palette_master."
  ))
}
# use only the colors needed for the detected communities
cluster_palette_use <- cluster_palette_master[cluster_ids]
# 5. ggnet2 plot
p <- ggnet2(
  net_plot,
  mode = xy,
  color = "walktrap_cluster",
  palette = cluster_palette_use,
  shape = "diagnosis",
  size = "strength",
  edge.size = 0.4,
  edge.alpha = 0.25,
  edge.color = "grey55",
  label = FALSE,
  legend.position = "right"
) +
  guides(size = "none") +
  ggtitle("Walktrap communities of EF subject-similarity network") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
print(p)
                 
# count members of cluster_allsubs (frequencies)
if(selection == 1){
  count_ones = sum(cluster_allsubs$membership == 1)
  count_twos = sum(cluster_allsubs$membership == 2)
  totalsubs = count_ones + count_twos
  freq_ones = count_ones / totalsubs
  freq_twos = count_twos / totalsubs
  cat("Group of ones (higher item values -BAD ER-) has", count_ones, "subjects,", freq_ones, "(frequency) \n")
  cat("Group of twos (lower item values -GOOD ER-) has", count_twos, "subjects,", freq_twos, "(frequency) \n")
}
if(selection == 2 && unique_clusters == 2) {
  names_ones <- cluster_allsubs$names[cluster_allsubs$membership == 1]
  names_twos <- cluster_allsubs$names[cluster_allsubs$membership == 2]
  count_ones_a <- sum(grepl("^a", names_ones))
  count_ones_c <- sum(grepl("^c", names_ones))
  count_twos_a <- sum(grepl("^a", names_twos))
  count_twos_c <- sum(grepl("^c", names_twos))
  count_ones = count_ones_a + count_ones_c
  count_twos = count_twos_a + count_twos_c
  totalsubs = count_ones + count_twos
  freq_ones = count_ones / totalsubs
  freq_twos = count_twos / totalsubs
  cat("Group of ones (higher item values -Worse EF-) has", count_ones, "subjects,", freq_ones, "(frequency) \n")
  cat("    In this cluster,", count_ones_a, "were diagnosed with ADHD \n")
  cat("    In this cluster,", count_ones_c, "were considered TD \n")
  cat("Group of twos (lower item values -Better EF-) has", count_twos, "subjects,", freq_twos, "(frequency) \n")
  cat("    In this cluster,", count_twos_a, "were diagnosed with ADHD \n")
  cat("    In this cluster,", count_twos_c, "were considered TD \n")
} 
if(selection == 2 && unique_clusters == 3) {
  names_ones <- cluster_allsubs$names[cluster_allsubs$membership == 1]
  names_twos <- cluster_allsubs$names[cluster_allsubs$membership == 2]
  names_three <- cluster_allsubs$names[cluster_allsubs$membership == 3]
  count_ones_a <- sum(grepl("^a", names_ones))
  count_ones_c <- sum(grepl("^c", names_ones))
  count_twos_a <- sum(grepl("^a", names_twos))
  count_twos_c <- sum(grepl("^c", names_twos))
  count_threes_a <- sum(grepl("^a", names_three))
  count_threes_c <- sum(grepl("^c", names_three))
  count_ones = count_ones_a + count_ones_c
  count_twos = count_twos_a + count_twos_c
  count_threes = count_threes_a + count_threes_c
  totalsubs = count_ones + count_twos + count_threes
  freq_ones = count_ones / totalsubs
  freq_twos = count_twos / totalsubs
  freq_threes = count_threes / totalsubs
  cat("Cluster 1 has", count_ones, "subjects,", freq_ones, "(frequency) \n")
  cat("    In this cluster,", count_ones_a, "were diagnosed with ADHD \n")
  cat("    In this cluster,", count_ones_c, "were considered TD \n")
  cat("Cluster 2 has", count_twos, "subjects,", freq_twos, "(frequency) \n")
  cat("    In this cluster,", count_twos_a, "were diagnosed with ADHD \n")
  cat("    In this cluster,", count_twos_c, "were considered TD \n")
  cat("Cluster 3 has", count_threes, "subjects,", freq_threes, "(frequency) \n")
  cat("    In this cluster,", count_threes_a, "were diagnosed with ADHD \n")
  cat("    In this cluster,", count_threes_c, "were considered TD \n")
}
if(selection == 2 && unique_clusters == 4) {
  names_ones <- cluster_allsubs$names[cluster_allsubs$membership == 1]
  names_twos <- cluster_allsubs$names[cluster_allsubs$membership == 2]
  names_three <- cluster_allsubs$names[cluster_allsubs$membership == 3]
  names_four <- cluster_allsubs$names[cluster_allsubs$membership == 4]
  count_ones_a <- sum(grepl("^a", names_ones))
  count_ones_c <- sum(grepl("^c", names_ones))
  count_twos_a <- sum(grepl("^a", names_twos))
  count_twos_c <- sum(grepl("^c", names_twos))
  count_threes_a <- sum(grepl("^a", names_three))
  count_threes_c <- sum(grepl("^c", names_three))
  count_fours_a <- sum(grepl("^a", names_four))
  count_fours_c <- sum(grepl("^c", names_four))   
  count_ones = count_ones_a + count_ones_c
  count_twos = count_twos_a + count_twos_c
  count_threes = count_threes_a + count_threes_c
  count_fours = count_fours_a + count_fours_c
  totalsubs = count_ones + count_twos + count_threes + count_fours
  freq_ones = count_ones / totalsubs
  freq_twos = count_twos / totalsubs
  freq_threes = count_threes / totalsubs
  freq_fours = count_fours / totalsubs
  cat("Cluster 1 has", count_ones, "subjects,", freq_ones, "(frequency) \n")
  cat("    In this cluster,", count_ones_a, "were diagnosed with ADHD \n")
  cat("    In this cluster,", count_ones_c, "were considered TD \n")
  cat("Cluster 2 has", count_twos, "subjects,", freq_twos, "(frequency) \n")
  cat("    In this cluster,", count_twos_a, "were diagnosed with ADHD \n")
  cat("    In this cluster,", count_twos_c, "were considered TD \n")
  cat("Cluster 3 has", count_threes, "subjects,", freq_threes, "(frequency) \n")
  cat("    In this cluster,", count_threes_a, "were diagnosed with ADHD \n")
  cat("    In this cluster,", count_threes_c, "were considered TD \n")
  cat("Cluster 4 has", count_fours, "subjects,", freq_fours, "(frequency) \n")
  cat("    In this cluster,", count_fours_a, "were diagnosed with ADHD \n")
  cat("    In this cluster,", count_fours_c, "were considered TD \n")  
}

# save the Walktrap output as a graph file and as RData file
if (selection == 1){
  V(net_allsubs)$comm = membership(cluster_allsubs)
  write_graph(net_allsubs, file = "net_allfeatures_ADHDonly.gml", format = "gml")
  save(cluster_allsubs, file = "clustersdata_ADHDonly.RData")
}
if (selection == 2 && unique_clusters == 2){
  V(net_allsubs)$comm = membership(cluster_allsubs)
  write_graph(net_allsubs, file = "net_allfeatures_ADHDandTD.gml", format = "gml")
  save(cluster_allsubs, file = "clustersdata_ADHDandTD.RData")
} 
if (selection == 2 && unique_clusters == 3){
  V(net_allsubs)$comm = membership(cluster_allsubs)
  write_graph(net_allsubs, file = "Three_clusters_net_allfeatures_ADHDandTD.gml", format = "gml")
  save(cluster_allsubs, file = "Three_clusters_data_ADHDandTD.RData")
  writeMat("Three_clusters_membership_ADHDandTD.mat",
           membership = cluster_allsubs$membership,
           names = cluster_allsubs$names)
} 
if(selection == 2 && unique_clusters == 4){
  V(net_allsubs)$comm = membership(cluster_allsubs)
  write_graph(net_allsubs, file = "Four_clusters_net_allfeatures_ADHDandTD.gml", format = "gml")
  save(cluster_allsubs, file = "Four_clusters_data_ADHDandTD.RData")
  writeMat("Four_clusters_membership_ADHDandTD.mat",
           membership = cluster_allsubs$membership,
           names = cluster_allsubs$names)
}
