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
quanteda_options("threads" = 6)


# Part 1)-----------------------------------------------------------------------

# Get tokenized and counted data from scratch or saved object
tokens.level <- 5L
ngram.table <- GetDataFrom("scratch", n.lines = 10000L, n.max = tokens.level,
                           min.occurances = 1L)

# Define input text to predict from
text.input <- "where in the world is"
prefix.words <- str_split(text.input, pattern = " ") %>%
  unlist()

prediction <- PredictWords(ngram.table = ngram.table,
                           prefix.words = prefix.words,
                           order.maximum = tokens.level, discount = 0.4)
print(prediction)

