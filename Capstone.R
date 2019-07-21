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
library(quanteda)
library(magrittr)
source("capstone_functions.R")


# Part 1)-----------------------------------------------------------------------

# Read in text, convert to corpus, and then convert to tokens
ngrams <- AssembleCorpus(n.lines = 10) %>%
  TokenizeAndClean()

# Convert tokens object to table and perform calculations
ngrams <- ngrams %>%
  dfm() %>%
  textstat_frequency() %>%
  as_tibble() %>%
  select(feature, frequency) %>%
  mutate(frequency = as.integer(frequency)) %>%
  mutate(n = CountGrams(feature)) %>%
  ApplyWCAttribute() %>%
  ApplyQMLs()

# Set the discount values to use
d2 <- 0.5
d3 <- 0.5

# This is the first step that requires discount values
ngrams <- ngrams %>%
  ApplyAdjFreq(discount.bis = d2, discount.tris = d3) %>%
  ApplyAlphaWords(discount.bis = d2, discount.tris = d3) %>%
  ApplyAlphaValues(discount.bis = d2, discount.tris = d3)

# This is the first step that requires knowledge of the preceding word(s)
previous.words <- c("has", "been")
ngrams <- ApplyQboValues(ngrams, preceding.words = previous.words)

# This is the "prediction" step
ngrams %>%
  filter(n == 1) %>%
  select(feature, qbo.tri) %>%
  arrange(desc(qbo.tri)) %>%
  print()


# An error is thrown if one of the preceding words is not in the vocabulary

# An error is also thrown if one of the preceding bigram is unobserved





