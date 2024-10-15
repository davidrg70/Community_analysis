### dg_itemlevel_data_converter_ER ###
# David Garnica, UNC, September 2024

# NOTE: DO NOT BRING COMPOSITE SCORES IN THE EXCEL SHEETS, NOR AMONG ITEMS NOR AT THE ENDS OF THE TABLE!!
# Still, I coded a way to handle possible composites, removing columns with keywords (see code lines down)

# install.packages("tidyverse")
# install.packages("numbers")
# install.packages("cheapr")
# rm(list = ls())

# Bring data from excel sheet
directory <- "/users/d/g/dga/BrainMAP/"
setwd(directory)
data <- readxl::read_xlsx("RedCap_September2024_ER-EF-ADHD.xlsx", sheet = "p_all_ER", range="A1:BL246")

# Trim table/data --------------------------------------------------------------------------------------------------------------------------------------
# First, determine BrainMAP waves and remove the related column
second_column <- data[[2]]  # Extract the second column of the table
wave_patterns <- c() # Initialize a list to store unique 'wave' patterns
# Loop through each row in the second column
for (row in second_column) {
  # Check if the row contains the word "wave"
  if (grepl("wave[0-9]+", row)) {
    wave_pattern <- sub("^(wave[0-9]+).*", "\\1", row) # Extract the text pattern containing 'wave0n'
    # Add the pattern to the list if it's not already there
    if (!(wave_pattern %in% wave_patterns)) {
      wave_patterns <- c(wave_patterns, wave_pattern)
    }
  }
}
waves <- wave_patterns # Output the unique wave patterns found
data <- data[ ,-2] # Removes the second column of the table

# Second, determine all columns names and, thus, surveys
column_names <- colnames(data) # Extract column names from the dataset
patterns_found <- c() # Initialize a list to store unique patterns found after 'p_'
# Loop through the column names
for (col_name in column_names) {
  # Check if the column name contains 'p_' followed by any pattern
  if (grepl("^p_", col_name)) {
    pattern <- sub("^p_([a-zA-Z0-9]+).*", "\\1", col_name) # Extract the pattern after 'p_' using sub and regular expressions
    # Add the pattern to the list if it's not already there
    if (!(pattern %in% patterns_found)) {
      patterns_found <- c(patterns_found, pattern)
    }
  }
}
surveys <- patterns_found # Output the unique patterns found, the surveys names

# Every subject can have a different number of rows, get subject_rows
subject_column <- data[[1]]  # Extract the first column with subject numbers
subject_rows <- list() # Initialize a list to store the row numbers for each subject
current_subject <- subject_column[1] # Track the current subject
rows_for_subject <- c(1)  # Start with the first row
# Loop through all rows starting from the second one
for (i in 2:nrow(data)) {
  # Check if the subject number in the current row is the same as the current subject
  if (is.na(subject_column[i]) || subject_column[i] == current_subject) {
    rows_for_subject <- c(rows_for_subject, i) # If it belongs to the same subject (or is NA), add the row to the current subject
  } else {
    subject_rows[[as.character(current_subject)]] <- rows_for_subject # If the subject number changes, store the rows for the current subject
    current_subject <- subject_column[i] # Update to the new subject and reset the row tracking
    rows_for_subject <- c(i)  # Start with the new row for the new subject
  }
}
subject_rows[[as.character(current_subject)]] <- rows_for_subject # Save the last subject's rows

# The program assumes that the data of every column of the 2nd row belongs to the same column of the 1st column.
# Only if the column of the 2nd row has data, and it is not NA, it is appended to the same column of the 1st!!
for (subject in names(subject_rows)) {
  rows <- subject_rows[[subject]]
  # Check if the subject has more than 1 row (we need at least 2 rows to concatenate)
  if (length(rows) > 1) {
    row1 <- rows[1] # first row of the subject
    row2 <- rows[2] # second row of the subject
    # Nested for loop to iterate through each column of row 2
    for (col in 1:ncol(data)) {
      if(is.character(data[[col]])) {
        value_row1 <- as.character(data[row1, col]) # value in row 1, column col (CONVERT TO CHARACTER)
        value_row2 <- as.character(data[row2, col]) # value in row 2, column col (CONVERT TO CHARACTER)
        # Check if the value in row 2 is not NA
        if (!is.na(value_row2) && value_row2 != "") {
          data[row1, col] <- paste(value_row1, value_row2, sep = " ") # concatenate the values/text and update the value in row 1
        }
      }
    }
  }
}

# Keep only the first row of every subject
rows_to_keep <- c() # Create a vector to store the rows we want to keep
# Loop through each subject and keep only the first row for each subject
for (subject in names(subject_rows)) {
  rows <- subject_rows[[subject]]
  rows_to_keep <- c(rows_to_keep, rows[1])  # Keep the first row of each subject
}
data <- data[rows_to_keep, ] # Now, subset the data to only include the rows we want to keep

# Check every column of every row (subject) and check if all the columns have NA. If so, remove that subject
rows_to_keep <- c() # Create a vector to store the rows we want to keep
# Loop through each row in the data
for (row in 1:nrow(data)) {
  total_columns <- ncol(data) - 1  # Ignore the first column
  na_count <- sum(is.na(data[row, 2:ncol(data)]))  # Only check from the second column onwards
  
  # If less than all relevant columns (from column 2 onwards) are NA, keep the row
  if (na_count < total_columns) {
    rows_to_keep <- c(rows_to_keep, row)  # Keep this row if not all relevant columns are NA
  }
}
data <- data[rows_to_keep, ] # Subset the data to only include the rows we want to keep

# To this point, all subjects have data in 1 row, and every column is well populated (all qualitative+quantitative data)
# Then, leave only the numeric values, and erase the text of every column (the survey's qualitative data)
for (i in 1:nrow(data)) {
  for (j in 2:ncol(data)) {
    # Check if the column is not already numeric
    if (!is.numeric(data[[j]])) {
      number <- sub(".*\\((\\d+)\\).*", "\\1", data[i, j]) # Extract the number within parentheses using regular expressions
      data[i, j] <- number  # Replace the content of the cell with the extracted number as character
    }
  }
}
data[ , 2:ncol(data)] <- lapply(data[ , 2:ncol(data)], function(x) as.numeric(as.character(x))) # Optionally convert columns to numeric after extraction
# Now 'data' contains only the numbers in columns 2 to the end

# Remove columns with composite scores (FILTERS BY KEYWORDS!) to leave only item-level data!!
keywords <- c("composite", "score", "total", "subscale", "raw")
columns_to_keep <-  !sapply(colnames(data), function(col_name) {
  any(sapply(keywords, function(keyword) grepl(keyword, col_name, ignore.case = TRUE)))
})
data <- data[ , columns_to_keep]

# Last trimming step is to remove text of subject_id column, leaving only the subject's numbers!
data$subject_id <- gsub("[^0-9]", "", data$subject_id)

# Determine unique numeric values and a LCM ----------------------------------------------------------------------------------------------------------------------
all_numbers <- c()
for (j in 2:ncol(data)) {
  for (i in 1:nrow(data)) {
    number <- sub(".*\\((\\d+)\\).*", "\\1", data[i,j]) # extract the number from the current cell using regex, ignoring columns names! as they have numbers
    if (grepl("\\d+", number)) {
      all_numbers <- c(all_numbers, as.numeric(number)) # if a number is found, add it to the vector
    }
  }
}
unique_numbers <- sort(unique(all_numbers))
print(paste("The unique Likert responses in the ingressed clinical data are: ", paste(unique_numbers, collapse = "")))

# CALCULATE A LEAST COMMON MULTIPLE (LCM) of all numbers in this vector
# but first, take out the 0 values, and include it later as the minimum range value
nonzero_numbers <- unique_numbers[unique_numbers != 0]
library(numbers)
data_lcm <- mLCM(c(nonzero_numbers))
print(paste("The Least Common Multiple of the unique Likert values is: ", paste(data_lcm, collapse = "")))

# Convert values / Re-scale!! --------------------------------------------------------------------------------------------------------------------------
# first, set a range of conversion and set a re-scaled numbers variable
if (0 %in% unique_numbers) {
  conversion_range <- c(0, data_lcm)
  num_elements <- length(unique_numbers)
  rescaled_numbers <- seq(from = 0, to = data_lcm, length.out = num_elements)
} else if (!(0 %in% unique_numbers)) {
  conversion_range <- c(min(unique_numbers), data_lcm)
  num_elements <- length(unique_numbers)
  rescaled_numbers <- seq(from = min(unique_numbers), to = data_lcm, length.out = num_elements)
}

print("The conversion range is: ")
cat(conversion_range, "\n")
print("The Likert values re-scaled are: ")
cat(conversion_range, "\n")

# second, convert all elements in the data frame/ table, accordingly
print("Now, converting all values in the table... ")
rescaled_data <- data # initialize frame with the same dimensions
for (row in 1:nrow(data)) {
  for (col in 2:ncol(data)) {
    original_value <- data[row, col]
    index <- match(original_value, unique_numbers)
    if (!is.na(index)) {
      rescaled_data[row, col] <- rescaled_numbers[index] # if a match is found, replace with re-scaled value
    }
  }
}

# third, save the re-scaled data with date and RStudio format :)
current_date <- Sys.Date()
file_name <- paste0("Rescaled_RC_data_", current_date, ".rds")
saveRDS(rescaled_data, file = file_name)
cat("Re-scaled data saved as: ", file_name, "\n")
cat("File saved at: ", directory, "\n")