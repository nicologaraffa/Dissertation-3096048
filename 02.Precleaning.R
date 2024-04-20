# This script cleans and preprocesses earnings call transcripts.
# It removes unnecessary sections from the text, retaining only the core content between
# specific keywords. Then it saves the cleaned content back
# to the [CLEAN] folder for further analysis.

library(stringr)

# Define a list of all the texts already downloaded in the [GROSS] folder
file_names <- list.files(gross_files_path, pattern = "*.txt", full.names = TRUE)

# Define the output folder of the cleaned transcripts
cleaned_files_path <- "Earnings call transcript Q1.15 - Q3.23 [CLEAN]/"

# Define an automated function to remove not needed text according to a repetitive pattern
clean_content <- function(text) {
  # 1) Everything before the first "good morning"
  start <- str_locate(text, regex("good morning", ignore_case = TRUE))[1]
  
  # 2) Everything after the last "thank you"
  end <- str_locate_all(text, regex("thank you", ignore_case = TRUE))
  if (length(end) > 0) {
    last_end <- max(end[[1]][, 2])
  } else {
    last_end <- nchar(text)
  }
  
  # Extraction of the comprised text
  if (!is.na(start) & start < last_end) {
    return(substr(text, start, last_end))
  } else {
    return(text)  # If a "good morning" is not found, leave the text as it is (manual cleaning)
  }
}

# Apply function to all transcripts in the previously-defined [GROSS] folder
for (file_name in file_names) {
  content <- readLines(file_name, warn = FALSE)
  content <- paste(content, collapse = "\n")
  content <- clean_content(content)
  file_name_only <- basename(file_name) # maintain the file names
  # Write the processed content in the folder designated for cleaned transcripts.
  writeLines(content, file.path(cleaned_files_path, file_name_only))
}
