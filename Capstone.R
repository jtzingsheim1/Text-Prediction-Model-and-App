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
chunk.size <- 5000  # Twitter file is about 2.4 million lines

# # Profile 1
# prof1 <- system.time(data.folder %>%
#   file.path("en_US", "en_US.twitter.txt") %>%  # Append subfolder and filename
#   readLines(n = chunk.size))

# Create a small subset of text to experiment with
my.text <- data.folder %>%
  file.path("en_US", "en_US.twitter.txt") %>%  # Append subfolder and filename
  readLines(n = chunk.size)

rm(DLAndUnzipData, data.folder, chunk.size)

# # Profile 2
# prof2 <- system.time(corpus(my.text))

# Turn text into corpus
my.corp <- corpus(my.text)
#rm(my.text)

# Tokenize and clean text
my.tkn1 <- tokens(my.corp, what = "word", remove_numbers = TRUE,
                 remove_punct = TRUE, remove_symbols = TRUE,
                 remove_twitter = TRUE, remove_hyphens = TRUE,
                 remove_url = TRUE, ngrams = 1, verbose = F)
#rm(my.corp)

# Build dfm of unigrams and convert to dataframe
my.unigram <- my.tkn1 %>%
  dfm() %>%
  textstat_frequency()

# Plot unigram frequency by index
plot(x = 1:nrow(my.unigram), y = my.unigram$frequency,
     xlab = "Word Index (sorted)", ylab = "Word Frequency [count]")

# Display the top ten unigrams and frequencies
my.unigram %>%
  select(feature:docfreq) %>%
  filter(rank < 11) %>%
  print()

# Check how many words are needed to cover 50 and 90 percent of occurances
occurances <- sum(my.unigram$frequency)  # Total count of word occurances
# Create a table that inclues the cumulative frequencies and fraction
frequencies <- my.unigram %>%
  mutate(cum.freq = cumsum(frequency), cum.frac = cum.freq / occurances) %>%
  select(cum.frac)
words.5 <- frequencies %>%
  filter(cum.frac <= 0.5) %>%
  nrow()
words.9 <- frequencies %>%
  filter(cum.frac <= 0.9) %>%
  nrow()
# Plot cumulative occurance fraction by word index
plot(x = 1:nrow(frequencies), y = frequencies$cum.frac,
     xlab = "Word Index (sorted)", ylab = "Cumulative Occurance Fraction")
rm(occurances, frequencies)



#rm(my.unigram)

# Convert the 1-gram tokens to 2-gram tokens
my.tkn2 <- tokens_ngrams(my.tkn1, n = 2)

# Build dfm of bigrams and convert to dataframe
my.bigram <- my.tkn2 %>%
  dfm() %>%
  textstat_frequency()
#rm(my.tkn2)

# # Plot bigram frequency by index
# plot(x = 1:nrow(my.bigram), y = my.bigram$frequency,
#      xlab = "Bigram Index (sorted)", ylab = "Bigram Frequency [count]")
#rm(my.bigram)

# Display the top ten bigrams and frequencies
my.bigram %>%
  select(feature:docfreq) %>%
  filter(rank < 11) %>%
  print()

# Convert the 1-gram tokens to 3-gram tokens
my.tkn3 <- tokens_ngrams(my.tkn1, n = 3)
#rm(my.tkn1)

# Build dfm of trigrams and convert to dataframe
my.trigram <- my.tkn3 %>%
  dfm(verbose = F) %>%
  textstat_frequency()
#rm(my.tkn3)

# # Plot trigram frequency by index
# plot(x = 1:nrow(my.trigram), y = my.trigram$frequency,
#      xlab = "Trigram Index (sorted)", ylab = "Trigram Frequency [count]")
#rm(my.trigram)

# Display the top ten trigrams and frequencies
my.trigram %>%
  select(feature:docfreq) %>%
  filter(rank < 11) %>%
  print()



# my.tkn2x <- tokens(my.text, what = "word", remove_numbers = TRUE,
#                   remove_punct = TRUE, remove_symbols = TRUE,
#                   remove_twitter = TRUE, remove_hyphens = TRUE,
#                   remove_url = TRUE, ngrams = 2, verbose = TRUE)

# my.tkn12 <- tokens(my.text, what = "word", remove_numbers = TRUE,
#                   remove_punct = TRUE, remove_symbols = TRUE,
#                   remove_twitter = TRUE, remove_hyphens = TRUE,
#                   remove_url = TRUE, ngrams = 1:2, verbose = TRUE)

# # Profile 4
# prof4 <- system.time(dfm(my.tkn))
# Total time was about 1.86s for 30k chunk @ word





# my.data12 <- my.tkn12 %>%
#   #tokens_remove(pattern = "*_*") %>%
#   dfm(remove = "*_*", verbose = TRUE) %>%
#   textstat_frequency()
# 
# my.data21 <- my.tkn12 %>%
#   #tokens_keep(pattern = "*_*") %>%
#   dfm(select = "*_*", verbose = TRUE) %>%
#   textstat_frequency()
# 
# my.datax <- my.tkn12 %>%
#   dfm(verbose = TRUE) %>%
#   textstat_frequency()

# print(head(my.data))
# print(tail(my.data))



# # Build bigram
# my.bigram <- tokens_ngrams(my.tkn, n = 2)




# Review criteria:
# - Does the link lead to an HTML page describing the exploratory analysis of
# the training data set?
# - Has the data scientist done basic summaries of the three files? Word counts,
# line counts and basic data tables?
# - Has the data scientist made basic plots, such as histograms to illustrate
# features of the data?
# - Was the report written in a brief, concise style, in a way that a non-data
# scientist manager could appreciate?












