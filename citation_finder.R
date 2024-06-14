#####MAKE SURE YOU HAVE A CLEAN WORD DOCUMENT THAT IS NOT TRACKING CHANGES AND HAS NO COMMENTS#####
# Install and load required packages
if (!require(officer)) install.packages("officer", dependencies = TRUE)
if (!require(pdftools)) install.packages("pdftools", dependencies = TRUE)
if (!require(stringr)) install.packages("stringr", dependencies = TRUE)

library(officer)
library(pdftools)
library(stringr)

# Define the path to your file
file_path <- "citation_finder_validation.docx" # Change to your file path

# Function to read text from a .docx file
read_docx_text <- function(file_path) {
  doc <- read_docx(file_path)
  doc_text <- docx_summary(doc)$text
  document <- paste(doc_text, collapse = " ")
  #document <- gsub("\n", " ", document)
  return(document)
}

# Function to read text from a PDF file
read_pdf_text <- function(file_path) {
  pdf_text <- pdf_text(file_path)
  document <- paste(pdf_text, collapse = " ")
  #document <- gsub("\n", " ", document)
  return(document)
}

# Determine file type and extract text accordingly
if (grepl("\\.docx$", file_path)) {
  document <- read_docx_text(file_path)
} else if (grepl("\\.pdf$", file_path)) {
  document <- read_pdf_text(file_path)
} else {
  stop("Unsupported file type. Please provide a .docx or .pdf file.")
}

# Define the components of the regex
author <- "(?:[A-Z][A-Za-zÀ-ÖØ-öø-ÿ'`-]*)"
#author <- "(?:[A-Z][A-Za-z'`-]+)"
etal <- "(?:et al\\.?)"
additional <- paste0("(?:,? (?:(?:and |& )?", author, "|", etal, "))")
year_num <- "(?:19|20)[0-9]{2}[a-z]?"  # Year with optional lowercase letter
page_num <- "(?:, p\\.? [0-9]+)?"  # Always optional
year <- paste0("(?:, *", year_num, page_num, "| *\\(", year_num, page_num, "\\)| ", year_num, ")")

# Combine the components into the final regex
regex <- paste0(author, additional, "*", year)

# Find all matches for the combined pattern
citations <- unlist(str_extract_all(document, regex))

# Function to normalize citations
normalize_citation <- function(citation) {
  # Remove surrounding parentheses and extra spaces
  citation <- str_remove_all(citation, "[\\(\\)]")
  citation <- str_trim(citation)
  
  # Standardize the format
  citation <- str_replace_all(citation, "\\s+", " ")
  citation <- str_replace_all(citation, ",", "")
  citation <- str_replace_all(citation, " & ", " and ")
  
  return(citation)
}

# Normalize citations
normalized_citations <- sapply(citations, normalize_citation)

# Remove duplicates and sort the citations
unique_citations <- sort(unique(normalized_citations))

# Print or save the unique citations
for (citation in unique_citations) {
  print(citation)
}

# Optionally save the unique citations to a file
writeLines(unique_citations, "validation.txt")
