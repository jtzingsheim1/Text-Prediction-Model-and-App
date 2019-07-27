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


# Predict from 5 grams ---------------------------------------------------------

# Get tokenized and counted data from scratch or saved object
tokens.level <- 5L

# ngram.table <- GetDataFrom("scratch", n.lines = 450000L, n.max = tokens.level,
#                            min.occurances = 2L,
#                            file.name = "ngram_table_450_4.Rdata")

ngram.table <- GetDataFrom("saved.object",
                           file.name = "ngram_table_350_5.Rdata")

# Define input text to predict from
prefix.words <- "where in the world is" %>%
  str_split(pattern = " ") %>%
  unlist()

message(Sys.time(), " begin predicting words")

prediction <- PredictWords(ngram.table = ngram.table,
                           prefix.words = prefix.words,
                           order.maximum = tokens.level - 1L, discount = 0.4)

message(Sys.time(), " prediction complete, ngram.object is ",
        format(object.size(ngram.table), units = "Mb"))

print(prediction)


# Predict from 4 grams ---------------------------------------------------------

# Get tokenized and counted data from scratch or saved object
tokens.level <- 4L

# ngram.table <- GetDataFrom("scratch", n.lines = 450000L, n.max = tokens.level,
#                            min.occurances = 2L,
#                            file.name = "ngram_table_450_4.Rdata")

ngram.table <- GetDataFrom("saved.object",
                           file.name = "ngram_table_350_5.Rdata")

# # Define input text to predict from
# prefix.words <- "this is" %>%
#   str_split(pattern = " ") %>%
#   unlist()

message(Sys.time(), " begin predicting words")

prediction <- PredictWords(ngram.table = ngram.table,
                           prefix.words = prefix.words,
                           order.maximum = tokens.level - 1L, discount = 0.4)

message(Sys.time(), " prediction complete, ngram.object is ",
        format(object.size(ngram.table), units = "Mb"))

print(prediction)


# how would the model react to punctuation?
# fasterword is not handling punctuation well
