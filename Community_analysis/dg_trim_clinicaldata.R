### dg_trim_clinicaldata ###
# David Garnica, UNC, October 2024
# clean data downloaded from RedCap: CBCL, BRIEF, and COLUMBIA ---- I try to generalize to any dataset.....

# Bring data from excel sheets (downloaded from RedCap)
directory <- "/users/d/g/dga/BrainMAP/"
setwd(directory)
data_cbcl <- read.csv("RedCap_p_CBCLallSubscales_2024-10-10.csv")
data_brief <- read.csv("RedCap_p_BRIEFonlySubscores_2024-10-10.csv")
data_columbia <- read.csv("RedCap_p_ColumbiaIS_2024-10-10.csv")

# remove rows with text elements in the subjets_id column (2004b, test_, etc)
cleaned_data_cbcl <- data_cbcl
for (i in 1:nrow(cleaned_data_cbcl)) {
  # Check if the element in the first column is text (non-numeric)
  if (!grepl("^[0-9]+$", cleaned_data_cbcl[i, 1])) {
    cleaned_data_cbcl[i, ] <- NA  # Mark rows to be removed
  }
}
cleaned_data_cbcl <- cleaned_data_cbcl[!is.na(cleaned_data_cbcl[, 1]), ] # Remove rows with NA (marked for removal)

cleaned_data_brief <- data_brief
for (i in 1:nrow(cleaned_data_brief)) {
  # Check if the element in the first column is text (non-numeric)
  if (!grepl("^[0-9]+$", cleaned_data_brief[i, 1])) {
    cleaned_data_brief[i, ] <- NA  # Mark rows to be removed
  }
}
cleaned_data_brief <- cleaned_data_brief[!is.na(cleaned_data_brief[, 1]), ] # Remove rows with NA (marked for removal)

cleaned_data_columbia <- data_columbia
for (i in 1:nrow(cleaned_data_columbia)) {
  # Check if the element in the first column is text (non-numeric)
  if (!grepl("^[0-9]+$", cleaned_data_columbia[i, 1])) {
    cleaned_data_columbia[i, ] <- NA  # Mark rows to be removed
  }
}
cleaned_data_columbia <- cleaned_data_columbia[!is.na(cleaned_data_columbia[, 1]), ] # Remove rows with NA (marked for removal)

# Remove rows containing any NA element (some of them are repeated rows of a subject, e.g., 2004 has data and 2004 has NAs)
cleaned_data_cbcl <- cleaned_data_cbcl[complete.cases(cleaned_data_cbcl), ]
cleaned_data_brief <- cleaned_data_brief[complete.cases(cleaned_data_brief), ]
cleaned_data_columbia <- cleaned_data_columbia[complete.cases(cleaned_data_columbia), ]

# Finally, remove any column named "Event.Name"
if ("Event.Name" %in% colnames(cleaned_data_cbcl)) {
  cleaned_data_cbcl <- cleaned_data_cbcl[, !colnames(cleaned_data_cbcl) %in% "Event.Name"]
}

if ("Event.Name" %in% colnames(cleaned_data_brief)) {
  cleaned_data_brief <- cleaned_data_brief[, !colnames(cleaned_data_brief) %in% "Event.Name"]
}

if ("Event.Name" %in% colnames(cleaned_data_columbia)) {
  cleaned_data_columbia <- cleaned_data_columbia[, !colnames(cleaned_data_columbia) %in% "Event.Name"]
}

# Remove unnecessary variables, to call this script from others and get just the cleaned/trimmed frames!
rm(i)
rm(directory)

data_cbcl <- cleaned_data_cbcl
data_brief <- cleaned_data_brief
data_columbia <- cleaned_data_columbia

rm(cleaned_data_cbcl)
rm(cleaned_data_brief)
rm(cleaned_data_columbia)