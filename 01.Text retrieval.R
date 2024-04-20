# The following code scrapes URLs from the SeekingAlpha Database, extracts
# relevant content based on specific date patterns and saves it into designated files in
# the [GROSS] folder for each company.

library(rvest)
library(xml2)
library(pdftools)

# URL extraction from SeekingAlpha database (UBS)
urls <- c('https://seekingalpha.com/article/3360155-ubs-groups-ubs-ceo-sergio-ermotti-on-q2-2015-results-earnings-call-transcript',
          'https://seekingalpha.com/article/3637406-ubs-groups-ubs-ceo-sergio-ermotti-on-q3-2015-results-earnings-call-transcript',
          'https://seekingalpha.com/article/3970586-ubs-groups-ubs-ceo-sergio-ermotti-on-q1-2016-results-earnings-call-transcript',
          'https://seekingalpha.com/article/3993870-ubs-groups-ubs-ceo-sergio-ermotti-on-q2-2016-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4016589-ubs-groups-ubs-ceo-sergio-ermotti-on-q3-2016-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4041582-ubs-groups-ubs-ceo-sergio-ermotti-on-q4-2016-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4066695-ubs-groups-ubs-ceo-sergio-ermotti-on-q1-2017-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4092344-ubs-groups-ubs-ceo-sergio-ermotti-on-q2-2017-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4117273-ubs-group-ags-ubs-ceo-sergio-ermotti-on-q3-2017-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4139200-ubs-group-ags-ubs-ceo-sergio-ermotti-on-q4-2017-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4165102-ubs-group-ags-ubs-ceo-sergio-ermotti-on-q1-2018-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4189686-ubs-group-ag-ubs-ceo-sergio-ermotti-on-q2-2018-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4214173-ubs-group-ag-ubs-ceo-sergio-ermotti-on-q3-2018-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4234715-ubs-group-ag-ubs-ceo-sergio-ermotti-on-q4-2018-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4256721-ubs-group-ag-ubs-ceo-sergio-ermotti-on-q1-2019-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4276705-ubs-group-ag-ubs-ceo-sergio-ermotti-on-q2-2019-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4298033-ubs-group-ag-ubs-ceo-sergio-ermotti-on-q3-2019-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4318035-ubs-group-ag-ubs-ceo-sergio-ermotti-on-q4-2019-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4340716-ubs-group-ag-ubs-ceo-sergio-ermotti-on-q1-2020-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4359586-ubs-group-ags-ubs-ceo-sergio-ermotti-on-q2-2020-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4379984-ubs-group-ag-ubs-ceo-sergio-ermotti-on-q3-2020-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4401163-ubs-group-ag-ubs-ceo-ralph-hamers-on-q4-2020-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4421660-ubs-group-ag-ubs-ceo-ralph-hamers-on-q1-2021-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4440052-ubs-group-ag-ubs-ceo-ralph-hamers-on-q2-2021-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4462093-ubs-group-ag-ubs-ceo-ralph-hamers-on-q3-2021-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4483228-ubs-group-ag-ubs-ceo-ralph-hamers-on-q4-2021-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4503529-ubs-group-ag-ubs-ceo-ralph-hamers-on-q1-2022-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4525844-ubs-group-ag-ubs-ceo-ralph-hamers-on-q2-2022-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4548859-ubs-group-ag-ubs-q3-2022-earnings-call-transcript',
          'https://seekingalpha.com/article/4573812-ubs-group-ag-ubs-q4-2022-earnings-call-transcript',
          'https://seekingalpha.com/article/4596588-ubs-group-ag-ubs-q1-2023-earnings-call-transcript',
          'https://seekingalpha.com/article/4632270-ubs-group-ag-ubs-q2-2023-earnings-call-transcript',
          'https://seekingalpha.com/article/4648455-ubs-group-ag-ubs-q3-2023-earnings-call-transcript',
          'https://seekingalpha.com/article/4667857-ubs-group-ag-ubs-q4-2023-earnings-call-transcript'
)

# Define a function to download the content of a web page and save it with a specific name.
scrape_and_save_page <- function(url) {
  # Extract the quarter and the year from the URL to assign a unique ID.
  # Search for the pattern representing the quarter and the year within the URL itself.
  matches <- regexpr("q[1-4]-20[0-9]{2}", url)
  data_info <- regmatches(url, matches)
  # Create objects by extracting the second, sixth, and seventh elements from data_info.
  quarter <- substr(data_info, 2, 2)
  year <- substr(data_info, 6, 7)
  
  # Use read_html to read the page and then extract the text with CSS selector ".ip_il".
  page <- read_html(url)
  text <- page %>%
    html_nodes('.ip_il') %>%
    html_text()
  
  # File name creation in the format "UBS_Q{quarter}_{year}.txt".
  file_name <- paste0(my_folder, "UBS_Q", quarter, "_", year, ".txt")
  writeLines(text, file_name)
}
# Apply to each url
lapply(urls, scrape_and_save_page)

# Exception for two transcripts not present in the DB-> Manual import via pdf from corporate sources.
convert_pdf_to_txt <- function(pdf_file, output_folder) {
  file_name <- tools::file_path_sans_ext(basename(pdf_file))
  text <- pdf_text(pdf_file)
  txt_file <- paste0(output_folder, file_name, ".txt")
  # Insert the text extracted from the pdf into the created txt object
  writeLines(text, txt_file)
}

# Store the extracted transcripts in "Earnings call transcript Q1.15 - Q3.23 [GROSS]" ready for further processings.
output_folder <- "Earnings call transcript Q1.15 - Q3.23 [GROSS]/"
pdf_files <- c("PDF UBS/UBS_Q1_15.pdf", "PDF UBS/UBS_Q4_15.pdf")
lapply(pdf_files, function(pdf_file) convert_pdf_to_txt(pdf_file, output_folder))

# URL extraction from SeekingAlpha database (GS)
urls <- c('https://seekingalpha.com/article/3075726-goldman-sachs-gs-management-on-q1-2015-results-earnings-call-transcript',
          'https://seekingalpha.com/article/3331805-the-goldman-sachs-group-gs-q2-2015-results-earnings-call-transcript',
          'https://seekingalpha.com/article/3575896-goldman-sachs-groups-gs-management-presents-q3-2015-results-earnings-call-transcript',
          'https://seekingalpha.com/article/3821986-goldman-sachs-group-gs-q4-2015-results-earnings-call-transcript',
          'https://seekingalpha.com/article/3966310-goldman-sachs-group-gs-q1-2016-results-earnings-call-transcript',
          'https://seekingalpha.com/article/3989543-goldman-sachs-groups-gs-on-q2-2016-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4012963-goldman-sachs-groups-gs-management-on-q3-2016-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4037714-goldman-sachs-groups-gs-q4-2016-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4063131-goldman-sachs-group-gs-on-q1-2017-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4088520-goldman-sachs-group-gs-q2-2017-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4114051-goldman-sachs-gs-q3-2017-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4138161-goldman-sachs-gs-q4-2017-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4163590-goldman-sachs-gs-q1-2018-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4187876-goldman-sachs-group-inc-gs-management-on-q2-2018-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4212635-goldman-sachs-groups-gs-management-on-q3-2018-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4233789-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q4-2018-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4254571-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q1-2019-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4275278-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q2-2019-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4296693-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q3-2019-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4317106-goldman-sachs-groups-gs-ceo-david-solomon-on-q4-2019-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4337816-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q1-2020-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4358696-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q2-2020-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4379082-goldman-sachs-gs-ceo-david-solomon-on-q3-2020-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4399743-goldman-sachs-group-inc-s-gs-ceo-david-solomon-on-q4-2020-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4419085-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q1-2021-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4439066-goldman-sachs-group-inc-s-gs-ceo-david-solomon-on-q2-2021-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4460134-goldman-sachs-group-inc-s-gs-ceo-david-solomon-on-q3-2021-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4480149-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q4-2021-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4501442-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q1-2022-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4524023-goldman-sachs-group-inc-gs-ceo-david-solomon-on-q2-2022-results-earnings-call-transcript',
          'https://seekingalpha.com/article/4547268-goldman-sachs-group-inc-gs-q3-2022-earnings-call-transcript',
          'https://seekingalpha.com/article/4570399-goldman-sachs-group-inc-gs-q4-2022-earnings-call-transcript',
          'https://seekingalpha.com/article/4594765-goldman-sachs-group-inc-gs-q1-2023-earnings-call-transcript',
          'https://seekingalpha.com/article/4618138-goldman-sachs-group-inc-gs-q2-2023-earnings-call-transcript',
          'https://seekingalpha.com/article/4641349-goldman-sachs-group-inc-gs-q3-2023-earnings-call-transcript',
          'https://seekingalpha.com/article/4663117-goldman-sachs-group-inc-gs-q4-2023-earnings-call-transcript'
)
# Function for web scraping a single page and saving the file with a specific name
scrape_and_save_page <- function(url) {
  # Extraction of the quarter and year automatically from the URL
  matches <- regexpr("q[1-4]-20[0-9]{2}", url)
  data_info <- regmatches(url, matches)
  quarter <- substr(data_info, 2, 2)
  year <- substr(data_info, 6, 7)

  # Reading the web page
  page <- read_html(url)
  text <- page %>%
    html_nodes('.w-full') %>%
    html_text()
  # Construction of the file name
  file_name <- paste0(output_folder, "GS_Q", quarter, "_", year, ".txt")
  writeLines(text, file_name)
}
