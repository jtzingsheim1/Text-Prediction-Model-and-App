# Coursera Data Science Specialization Capstone Project 1 Script----------------
# Milestone report for predictive text project


# The purpose of this script is to complete the basic requirements behind the
# first capstone project from Johns Hopkins University within the Data Science
# Specialization on Coursera.
#
# Teh review criteria for this part of the project are:
# - Does the link lead to an HTML page describing the exploratory analysis of
# the training data set?
# - Has the data scientist done basic summaries of the three files? Word counts,
# line counts and basic data tables?
# - Has the data scientist made basic plots, such as histograms to illustrate
# features of the data?
# - Was the report written in a brief, concise style, in a way that a non-data
# scientist manager could appreciate?
#
# This project will begin to meet the objectives by building a script to work
# with the data. This script will serve as the basis for an R Markdown file
# which will create the actual deliverable of the html report.


library(tidyverse)
#library(readtext)
library(quanteda)


# Part 0) Function definitions--------------------------------------------------

DLAndUnzipData <- function(data.filename = "Coursera-SwiftKey.zip") {
  # Downloads and unzips the capstone dataset if needed, returns folder name
  #
  # Args:
  #   data.filename: An optional name for the zip file, to replace the default
  #
  # Returns:
  #   A chacacter value of the name of the folder containing the data

  # Check if the file already exists, download if it does not
  if (!file.exists(data.filename)) {
    print("Downloading Data File")
    url <- paste0("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/",
                  "Coursera-SwiftKey.zip")
    download.file(url, data.filename)
  }
  
  # Check if the file is already unzipped, unzip if needed
  data.folder <- "final"
  if (!file.exists(data.folder)) {
    print("Unzipping Data File")
    unzip(data.filename)
  }
  return(data.folder)  # Return directory of unzipped file contents as character
}

AssembleCorpus <- function(n.lines,
                           file.selection = c("blogs", "news", "twitter"),
                           sub.dir = c("en_US", "de_DE", "fi_FI", "ru_RU")) {
  # Reads in specified number of lines from the specified file, assembles corpus
  #
  # Args:
  #   n.lines: The number of lines to read in from the text with readLines()
  #   file: Select which file to read from, one of: blogs, news, or twitter
  #   sub.dir: The subdirectory to read in files from, "en_US" by default
  #
  # Returns:
  #   A corpus of the text from the selected file, one "text" per line
  
  # Check and set arguments
  file.selection <- match.arg(file.selection)
  sub.dir <- match.arg(sub.dir)

  # Download and unzip the data, store folder name and file path
  filename <- paste(sub.dir, file.selection, "txt", sep = ".")  # Build file name
  filepath <- file.path(DLAndUnzipData(), sub.dir, filename)  # Build file path
  file.corpus <- filepath %>%
    readLines(n = n.lines) %>%  # Read in text
    corpus()  # Convert to corpus
  
  # Set metadata for the corpus
  docnames(file.corpus) <- paste0(file.selection, 1:ndoc(file.corpus))
  file.corpus$metadata$source <- filename
  file.corpus$metadata$file.size <- file.info(filepath)$size
  file.corpus$metadata$rows.read <- nrow(file.corpus$documents)
  
  # Return the corpus
  return(file.corpus)
}


# Part 1) Load and process the data---------------------------------------------

chunk.size <- 3

# Read in text data and assemble into corpora
blogs.corp <- AssembleCorpus(n.lines = chunk.size, file = "blogs")
news.corp <- AssembleCorpus(n.lines = chunk.size, file = "news")
twitter.corp <- AssembleCorpus(n.lines = chunk.size, file = "twitter")

# Tokenize and clean text
# The predictive model will not attempt to predict: numbers, punctuation,
# symbols, twitter handles, hyphens, or urls, so these are all removed
blogs.tkn <- tokens(blogs.corp, remove_numbers = T, remove_punct = T,
                    remove_symbols = T, remove_twitter = T, remove_hyphens = T,
                    remove_url = T)
news.tkn <- tokens(news.corp, remove_numbers = T, remove_punct = T,
                    remove_symbols = T, remove_twitter = T, remove_hyphens = T,
                    remove_url = T)
twitter.tkn <- tokens(twitter.corp, remove_numbers = T, remove_punct = T,
                    remove_symbols = T, remove_twitter = T, remove_hyphens = T,
                    remove_url = T)

# Count the number of words in each token object
blogs.wc <- blogs.tkn %>%
  dfm() %>%
  textstat_frequency() %>%
  select(frequency) %>%
  sum()

# # Build dfm of unigrams and convert to dataframe
# my.unigram <- my.tkn1 %>%
#   dfm() %>%
#   textstat_frequency()







# # Plot unigram frequency by index
# plot(x = 1:nrow(my.unigram), y = my.unigram$frequency,
#      xlab = "Word Index (sorted)", ylab = "Word Frequency [count]")
# 
# # Display the top ten unigrams and frequencies
# my.unigram %>%
#   select(feature:docfreq) %>%
#   filter(rank < 11) %>%
#   print()

# # Check how many words are needed to cover 50 and 90 percent of occurances
# occurances <- sum(my.unigram$frequency)  # Total count of word occurances
# # Create a table that inclues the cumulative frequencies and fraction
# frequencies <- my.unigram %>%
#   mutate(cum.freq = cumsum(frequency), cum.frac = cum.freq / occurances) %>%
#   select(cum.frac)
# words.5 <- frequencies %>%
#   filter(cum.frac <= 0.5) %>%
#   nrow()
# words.9 <- frequencies %>%
#   filter(cum.frac <= 0.9) %>%
#   nrow()
# # Plot cumulative occurance fraction by word index
# plot(x = 1:nrow(frequencies), y = frequencies$cum.frac,
#      xlab = "Word Index (sorted)", ylab = "Cumulative Occurance Fraction")
# rm(occurances, frequencies)
# #rm(my.unigram)

# # Convert the 1-gram tokens to 2-gram tokens
# my.tkn2 <- tokens_ngrams(my.tkn1, n = 2)
# 
# # Build dfm of bigrams and convert to dataframe
# my.bigram <- my.tkn2 %>%
#   dfm() %>%
#   textstat_frequency()
# #rm(my.tkn2)

# # Plot bigram frequency by index
# plot(x = 1:nrow(my.bigram), y = my.bigram$frequency,
#      xlab = "Bigram Index (sorted)", ylab = "Bigram Frequency [count]")
#rm(my.bigram)

# # Display the top ten bigrams and frequencies
# my.bigram %>%
#   select(feature:docfreq) %>%
#   filter(rank < 11) %>%
#   print()

# # Convert the 1-gram tokens to 3-gram tokens
# my.tkn3 <- tokens_ngrams(my.tkn1, n = 3)
# #rm(my.tkn1)
# 
# # Build dfm of trigrams and convert to dataframe
# my.trigram <- my.tkn3 %>%
#   dfm(verbose = FALSE) %>%
#   textstat_frequency()
# #rm(my.tkn3)

# # Plot trigram frequency by index
# plot(x = 1:nrow(my.trigram), y = my.trigram$frequency,
#      xlab = "Trigram Index (sorted)", ylab = "Trigram Frequency [count]")
#rm(my.trigram)

# # Display the top ten trigrams and frequencies
# my.trigram %>%
#   select(feature:docfreq) %>%
#   filter(rank < 11) %>%
#   print()















