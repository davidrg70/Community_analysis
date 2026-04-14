### dg_determine_groups ###
# David Garnica, UNC, September 2024, Updated February 2025

directory <- "/users/d/g/dga/BrainMAP/"
setwd(directory)
# The Demographics Form.csv is found in SharePoint: cohenlabteam/Documents/Research Studies/ADHD BrainMAP/Data/Demographics Form.csv
ids_data <- read.csv("Demographics Form.csv")
directory <- "/users/d/g/dga/BrainMAP/EF_data3/EF_clustering/"
setwd(directory)
data <- readRDS("Rescaled_EF_data_2026-04-14.rds")

# Subjects diagnosed with ADHD:
adhd_sub_ids <- ids_data$sub_id[ids_data$adhd_diag == "ADHD"]
# Subjects included as TD (Typically Developing):
td_sub_ids <- ids_data$sub_id[ids_data$adhd_diag == "TD"]
# Subjects in common: ADHD AND TD participants in data downloaded from RedCap
subjects = intersect(data$subject_id, ids_data$sub_id)

# from those collected, who are ADHD?
ADHDs = intersect(subjects, adhd_sub_ids)
# from those collected, who are TD?
TDs = intersect(subjects, td_sub_ids)
