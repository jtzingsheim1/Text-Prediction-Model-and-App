# Coursera Data Science Specialization Capstone Project Script------------------
# Predicting the next word in a sequence


# The purpose of this script is to complete the basic requirements behind the
# capstone project from Johns Hopkins University within the Data Science
# Specialization on Coursera.
#
# The instructions outline the objectives of the project as:
# - Analyze a large corpus of text documents
# - Discover structure in the data and how words are put together
# - Clean and analyze the text data
# - Build a predictive text model
# - Build a predictive text product that uses the model
#
# This project will begin to meet the objectives by building a script to work
# with the data and build a predictive model


library(tidyverse)
#library(readtext)
library(quanteda)


# Part 0) Function definitions--------------------------------------------------

DLAndUnzipData <- function(data.filename = "Coursera-SwiftKey.zip",
                           profanity.filename = "profanity_list.txt") {
  # Downloads and unzips the data if needed, returns dataset folder name
  #
  # Args:
  #   data.filename: An optional name for the zip file, to replace the default
  #
  # Returns:
  #   A chacacter value of the name of the folder containing the data

  # Check if the profanity file already exists, download if it does not
  if (!file.exists(profanity.filename)) {
    message("Downloading profanity file")
    url <- paste0("https://raw.githubusercontent.com/RobertJGabriel/",
                  "Google-profanity-words/master/list.txt")
    download.file(url, profanity.filename)
  }
  
  # Check if the data file already exists, download if it does not
  if (!file.exists(data.filename)) {
    message("Downloading data file")
    url <- paste0("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/",
                  "Coursera-SwiftKey.zip")
    download.file(url, data.filename)
  }
  
  # Check if the file is already unzipped, unzip if needed
  data.folder <- "final"
  if (!file.exists(data.folder)) {
    message("Unzipping data file")
    unzip(data.filename)
  }
  return(data.folder)  # Return directory of unzipped file contents as character
}

AssembleCorpus <- function(n.lines,
                           sub.dir = c("en_US", "de_DE", "fi_FI", "ru_RU")) {
  # Reads in specified number of lines from the subdirectory, assembles corpus
  #
  # Args:
  #   n.lines: The number of lines to read in from each text with readLines()
  #   sub.dir: The subdirectory to read in files from, "en_US" by default
  #
  # Returns:
  #   A corpus combining text from all the files, one text per line from file
  
  # Check and set arguments
  sub.dir <- match.arg(sub.dir)

  # Download and unzip the data, store folder name and file paths
  data.folder <- file.path(DLAndUnzipData(), sub.dir)
  file.names <- list.files(data.folder)  # Collect files names
  file.paths <- file.path(data.folder, file.names)  # Append file names to path
  
  # Read in the data and combine into a single corpus
  text.list <- map(file.paths, readLines, n = n.lines)
  blogs.corp <- corpus(text.list[[1]])
  news.corp <- corpus(text.list[[2]])
  twitter.corp <- corpus(text.list[[3]])
  full.corpus <- blogs.corp + news.corp + twitter.corp
  
  return(full.corpus)
}


# Part 1) Load and process the data---------------------------------------------

# Read in text data and assemble into corpus
corpus.full <- AssembleCorpus(n.lines = 100)

# Tokenize and clean text
# The predictive model will not attempt to predict: numbers, punctuation,
# symbols, twitter handles, hyphens, or urls, so these are all removed
tkns.1 <- tokens(corpus.full, what = "word", remove_numbers = TRUE,
                 remove_punct = TRUE, remove_symbols = TRUE,
                 remove_twitter = TRUE, remove_hyphens = TRUE,
                 remove_url = TRUE, ngrams = 1, verbose = FALSE)
# The tokens function runs faster and produces a smaller object if all n-grams
# are created at once, but can the model be built from that?

# Remove profanity from unigrams
profanities <- readLines("profanity_list.txt")
prof.test1 <- tkns.1 %>%
  tokens_keep(pattern = profanities) %>%
  dfm() %>%
  rowSums()

profanity.texts <- prof.test1[prof.test1 > 0] %>% names()
profanity.test <- prof.test1 > 0

tkns.1a <- tkns.1 %>%
  tokens_subset(subset = profanity.test)

tkns.2 <- tkns.1a %>%
  tokens_ngrams(n = 2L)

tkns.2a <- tkns.1a %>%
  tokens_remove(pattern = profanities) %>%
  tokens_ngrams(n = 2L)
  
tkns.2b <- tkns.1a %>%
  tokens_remove(pattern = profanities, padding = TRUE) %>%
  tokens_ngrams(n = 2L)


# tkns.test2 <- tkns.1 %>%
#   tokens_keep(pattern = profanities) %>%
#   dfm() %>%
#   textstat_frequency()


# # Build dfm of unigrams and convert to dataframe
# unigram <- all.tkn %>%
#   dfm() %>%
#   textstat_frequency()























