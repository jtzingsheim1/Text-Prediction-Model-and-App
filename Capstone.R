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

DLAndUnzipData <- function() {
  # Downloads and unzips the capstone dataset if needed, returns folder name
  #
  # Args:
  #   None
  #
  # Returns:
  #   The name of the folder containing the data in the current directory
  data.filename <- "Coursera-SwiftKey.zip"
  # Check if the file already exists, download if it does not
  if (!file.exists(data.filename)) {
    print("Downloading Data File")
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url, data.filename)
  }
  
  # Check if the file is already unzipped, unzip if needed
  data.folder <- "final"
  if (!file.exists(data.folder)) {
    print("Unzipping Data File")
    unzip(data.filename)
  }
  data.folder
}


# Part 1) Load and process the data---------------------------------------------

# Download and unzip the data, store folder name as object
data.folder <- DLAndUnzipData()
chunk.size <- 5

# Create a small subset of text to experiment with
my.text <- data.folder %>%
  file.path("en_US", "en_US.twitter.txt") %>%  # Append subfolder and filename
  readLines(n = chunk.size)




my.corp <- corpus(my.text)
#my.tkn.sent <- tokens(my.text, what = "sentence")
my.tkn.word <- tokens(my.text, what = "word", remove_numbers = TRUE,
                      remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_select(pattern = stopwords('en'), selection = 'remove')
my.dfm <- dfm(my.corp)
my.df <- convert(my.dfm, to = "matrix") %>%
  colSums()

my.text2 <- data.folder %>%
  paste0("/en_US/en_US.twitter.txt") %>%  # Append the subfolder and filename
  readLines(n = chunk.size)



# Project instructions:
# Exploratory analysis - perform a thorough exploratory analysis of the data,
# understanding the distribution of words and relationship between the words in
# the corpora. Understand frequencies of words and word pairs - build figures
# and tables to understand variation in the frequencies of words and word pairs
# in the data.














