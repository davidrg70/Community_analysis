### dg_determine_groups ###
# David Garnica, UNC, September 2024

directory <- "/users/d/g/dga/BrainMAP/"
setwd(directory)
ids_data <- read.csv("Demographics Form.csv")
data <- readRDS("Rescaled_RC_data_2024-10-11.rds")

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