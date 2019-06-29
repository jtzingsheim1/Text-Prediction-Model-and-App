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
chunk.size <- 10000  # Twitter file is about 2.4 million lines

# # Profile 1
# prof1 <- system.time(data.folder %>%
#   file.path("en_US", "en_US.twitter.txt") %>%  # Append subfolder and filename
#   readLines(n = chunk.size))

# Create a small subset of text to experiment with
my.text <- data.folder %>%
  file.path("en_US", "en_US.twitter.txt") %>%  # Append subfolder and filename
  readLines(n = chunk.size)

# # Profile 2
# prof2 <- system.time(corpus(my.text))

# Turn text into corpus
my.corp <- corpus(my.text)

# # Profile 3, this is the slow step
# prof3a <- system.time(tokens(my.text, what = "word", remove_numbers = TRUE,
#                             remove_punct = TRUE, remove_symbols = TRUE,
#                             remove_twitter = TRUE, remove_url = TRUE) %>%
#                        tokens_select(pattern = stopwords('en'),
#                                      selection = 'remove'))

# Tokenize and clean text
my.tkn <- tokens(my.text, what = "word", remove_numbers = TRUE,
                 remove_punct = TRUE, remove_symbols = TRUE,
                 remove_twitter = TRUE, remove_url = TRUE) %>%
  tokens_select(pattern = stopwords('en'), selection = 'remove')
#length(stopwords("english"))  # 175L

# # Profile 4
# prof4 <- system.time(dfm(my.tkn))
# Total time was about 1.86s for 30k chunk @ word

# Build dfm
my.dfm <- dfm(my.tkn)

# Check number of features and their frequency
my.feat <- nfeat(my.dfm)
my.data <- textstat_frequency(my.dfm)
print(head(my.data))
print(tail(my.data))


lines(x = 1:my.feat, y = my.data$frequency)







# Project instructions:
# Exploratory analysis - perform a thorough exploratory analysis of the data,
# understanding the distribution of words and relationship between the words in
# the corpora. Understand frequencies of words and word pairs - build figures
# and tables to understand variation in the frequencies of words and word pairs
# in the data.

# - Some words are more frequent than others - what are the distributions of
# word frequencies?
# - What are the frequencies of 2-grams and 3-grams in the dataset?
# - How many unique words do you need in a frequency sorted dictionary to cover
# 50% of all word instances in the language? 90%?
# - How do you evaluate how many of the words come from foreign languages?
# - Can you think of a way to increase the coverage -- identifying words that
# may not be in the corpora or using a smaller number of words in the dictionary
# to cover the same number of phrases?












