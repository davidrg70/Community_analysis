### dg_determine_age ###
# David Garnica, UNC, January 2025
# the script runs dg_trim_clinicaldata and reads the Demographic_Form.csv to calculate age of all BrainMAP participants available
rm(list = ls())
library(dplyr)

directory <- "/users/d/g/dga/BrainMAP/"
setwd(directory)

# Bring cleaned data, downloaded from RedCap
source("dg_trim_clinicaldata.R") # get trimmed data and subjects IDs in groups

# The Demographics Form.csv is found in SharePoint: cohenlabteam/Documents/Research Studies/ADHD BrainMAP/Data/Demographics Form.csv
ids_data <- read.csv("Demographics Form.csv")

### First, get a unique array of subject_ids, taking all the data frames (brief,cbcl,columbia)
subject_ids <- c(data_brief$Subject.ID, data_cbcl$Subject.ID, data_columbia$Subject.ID)
unique_subject_ids <- unique(subject_ids)

### Second, take the survey date of every subject, but check if the data is the same across the 3 data frames. If not, then take the oldest
combined_data <- bind_rows(
  data_brief %>% select(Subject.ID, Survey.Timestamp),
  data_cbcl %>% select(Subject.ID, Survey.Timestamp),
  data_columbia %>% select(Subject.ID, Survey.Timestamp)
)

combined_data <- combined_data %>%
  mutate(Survey.Timestamp = as.POSIXct(Survey.Timestamp, format = "%Y-%m-%d %H:%M:%S")) # convert Survey.Timestamp to a date-time object

result <- combined_data %>%
  filter(!is.na(Survey.Timestamp)) %>% # remove rows with NA in Survey.Timestamp
  group_by(Subject.ID) %>%
  summarize(Oldest.Timestamp = min(Survey.Timestamp)) %>%  # Find the oldest timestamp
  ungroup() # filter for unique Subject.IDs and find the oldest timestamp for each subject

### Third, calculate Age for each subject, taking Oldest.Timestamp and birthdate columns!!
# ensure both IDs are of the same type
result <- result %>%
  mutate(Subject.ID = as.character(Subject.ID))  # convert to character

ids_data <- ids_data %>%
  mutate(sub_id = as.character(sub_id))  # convert to character

# join 'result' with 'ids_data' based on Subject IDs
merged_data <- result %>%
  inner_join(ids_data, by = c("Subject.ID" = "sub_id"))

# ensure 'birthdate' is in a consistent date format
merged_data <- merged_data %>%
  mutate(birthdate = as.Date(birthdate, format = "%m/%d/%y"))

# calculate the age at the time of Oldest.Timestamp
merged_data <- merged_data %>%
  mutate(Age = as.numeric(difftime(Oldest.Timestamp, birthdate, units = "days")) / 365.25)

AgePerSubject <- merged_data %>%
  select(Subject.ID, Age)

# output the merged data with age and any discrepancies
print("Merged Data with Age:")
print(AgePerSubject)

# report any discrepancies (subjects in result not found in ids_data)
missing_subjects <- setdiff(result$Subject.ID, ids_data$sub_id)

if (length(missing_subjects) > 0) {
  print("Subjects in 'result' but not in ids_data:")
  print(missing_subjects)
} else {
  print("No discrepancies found. All subjects matched.")
}