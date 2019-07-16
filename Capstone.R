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
library(magrittr)


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
corpus.full <- AssembleCorpus(n.lines = 1)
rm(AssembleCorpus, DLAndUnzipData)

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
tkns.1 <- tokens_remove(tkns.1, pattern = profanities, padding = TRUE)
rm(profanities)


# Part 2) Build Models----------------------------------------------------------

rm(corpus.full, tkns.1)

corpus1 <- c("one two three four",
             "two four",
             "three four",
             "three four")
d2 <- 0.5
d3 <- 0.7

unigram.prefix <- "two"
#bigram.prefix <- c("one", "two")

unigrams <- corpus1 %>%
  tokens(n = 1) %>%
  dfm() %>%
  textstat_frequency() %>%
  select(feature, frequency) %>%
  mutate(n = 1)

word.count <- sum(unigrams$frequency)
word.list <- unigrams$feature

unigrams <- unigrams %>%
  mutate(qml = frequency / word.count)
  
bigrams <- corpus1 %>%
  tokens(n = 2)

alphas1 <- function(unigram, bigram.tkns) {
  pattern1 <- paste0(unigram, "_*")
  bigram.matches <- bigram.tkns %>%
    tokens_select(pattern = pattern1) %>%
    as.character() %>%
    str_remove(pattern = pattern1) %>%
    unique()
  
  # Test if vector is empty, and replace with NA if TRUE
  if (length(bigram.matches) == 0) {
    bigram.matches <- NA
  }
  
  return(bigram.matches)
}

betas1 <- function(alphas, vocabulary) {
  na.test <- sum(is.na(alphas))

  # Test if alphas is NA
  if (na.test != 0) {
    betas <- vocabulary
  } else {
    betas <- vocabulary %>%
      tokens(n = 1) %>%
      tokens_remove(pattern = alphas) %>%
      as.character()
  }
  
  return(betas)
}

alphas.1 <- map(unigrams$feature, alphas1, bigram.tkns = bigrams)
betas.1 <- map(alphas.1, betas1, vocabulary = word.list)

unigrams <- unigrams %>%
  mutate(alpha.words = alphas.1) %>%
  mutate(beta.words = betas.1)
rm(alphas.1, betas.1)

beta.qsum <- function(beta.features, unigram.table) {
  beta.qml.sum <- unigram.table %>%
    filter(feature %in% beta.features) %>%
    select(qml) %>%
    sum()

  return(beta.qml.sum)
}

beta.qsums <- map(unigrams$beta.words, beta.qsum, unigram.table = unigrams)

unigrams <- unigrams %>%
  mutate(beta.q.sums = beta.qsums)
rm(beta.qsums)

alphas1a <- function(unigram, bigram.tkns, discount) {
  pattern1 <- paste0(unigram, "_*")
  bigram.matches <- bigram.tkns %>%
    tokens_select(pattern = pattern1)
  bigram.test <- bigram.matches %>%
    as.character() %>%
    length()

  # Test if vector is empty, and replace with NA if TRUE
  if (bigram.test == 0) {
    alpha.sums <- NA
  } else {
    bigram.matches <- bigram.matches %>%
      dfm() %>%
      textstat_frequency() %$%
      frequency

    alpha.sums <- discount * length(bigram.matches) / sum(bigram.matches)
  }

  return(alpha.sums)
}

alpha.values <- map(unigrams$feature, alphas1a, bigram.tkns = bigrams,
                    discount = d2)

unigrams <- unigrams %>%
  mutate(alpha.value = alpha.values)

bigrams <- bigrams %>%
  dfm() %>%
  textstat_frequency() %>%
  select(feature, frequency) %>%
  mutate(n = 2) %>%
  mutate(adj.freq = frequency - d2)

trigrams <- corpus1 %>%
  tokens(n = 3)

alphas2 <- function(bigram, trigram.tkns) {
  pattern2 <- paste0(bigram, "_*")
  trigram.matches <- trigram.tkns %>%
    tokens_select(pattern = pattern2) %>%
    as.character() %>%
    str_remove(pattern = pattern2) %>%
    unique()
  
  # Test if vector is empty, and replace with NA if TRUE
  if (length(trigram.matches) == 0) {
    trigram.matches <- NA
  }
  
  return(trigram.matches)
}

alphas.2 <- map(bigrams$feature, alphas2, trigrams)
betas.2 <- map(alphas.2, betas1, vocabulary = word.list)

bigrams <- bigrams %>%
  mutate(alpha.words = alphas.2) %>%
  mutate(beta.words = betas.2)
rm(alphas.2, betas.2)

alphas2a <- function(bigram, trigram.tkns, discount) {
  pattern2 <- paste0(bigram, "_*")
  trigram.matches <- trigram.tkns %>%
    tokens_select(pattern = pattern2)
  trigram.test <- trigram.matches %>%
    as.character() %>%
    length()
  
  # Test if vector is empty, and replace with NA if TRUE
  if (trigram.test == 0) {
    alpha.sums <- NA
  } else {
    trigram.matches <- trigram.matches %>%
      dfm() %>%
      textstat_frequency() %$%
      frequency
    
    alpha.sums <- discount * length(trigram.matches) / sum(trigram.matches)
  }
  
  return(alpha.sums)
}

alpha.values2 <- map(bigrams$feature, alphas2a, trigrams, discount = d3)

bigrams <- bigrams %>%
  mutate(alpha.value = alpha.values2)
rm(alpha.values, alpha.values2)

trigrams <- trigrams %>%
  dfm() %>%
  textstat_frequency() %>%
  select(feature, frequency) %>%
  mutate(n = 3) %>%
  mutate(adj.freq = frequency - d3)

QboBigram <- function(word, preceding.word, unigram.table, bigram.table) {
  alpha.words1 <- unigram.table %>%
    filter(feature == preceding.word) %$%
    alpha.words[[1]]

  alpha.test <- word %in% alpha.words1
  
  if (alpha.test) {
    bigram <- paste(preceding.word, word, sep = "_")
    big.adj.freq <- bigram.table %>%
      filter(feature == bigram) %$%
      adj.freq
    uni.freq <- unigram.table %>%
      filter(feature == preceding.word) %$%
      frequency
    qbo.value <- big.adj.freq / uni.freq
  } else {
    alpha.value <- unigram.table %>%
      filter(feature == preceding.word) %$%
      alpha.value[[1]]
    qml.w <- unigram.table %>%
      filter(feature == word) %$%
      qml
    beta.q.sums.w <- unigram.table %>%
      filter(feature == preceding.word) %$%
      beta.q.sums[[1]]
    qbo.value <- alpha.value * qml.w / beta.q.sums.w
  }

  return(qbo.value)
  
}

uni.qbos <- map(unigrams$feature, QboBigram, preceding.word = unigram.prefix,
                unigram.table = unigrams, bigram.table = bigrams)

unigrams <- unigrams %>%
  mutate(qbos = uni.qbos)

beta.qbosum <- function(beta.features, unigram.table) {
  beta.qbo.sum <- unigram.table %>%
    filter(feature %in% beta.features) %$%
    qbos %>%
    as.numeric() %>%
    sum()
  
  return(beta.qbo.sum)
}

beta.qbosums <- map(bigrams$beta.words, beta.qbosum, unigram.table = unigrams)

unigrams <- unigrams %>%
  mutate(beta.qbo.sums = beta.qbosums)
rm(uni.qbos, beta.qbosums)


# The last step is to build the qbo function for trigrams







