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
source("TextPredictorApp/global.R")  # Contains global functions for app


# Script Version of App --------------------------------------------------------

# Load data from disk
ngram.table <- GetDataFrom("saved.object",
                           file.name = "TextPredictorApp/ngram_table.Rdata")

# Enter text to predict from
prefix.words <- PrepareInputText("Where in the world is")

# Profiling message
# message(Sys.time(), " begin predicting words")

# Make prediction
prediction <- PredictWords(ngram.table = ngram.table,
                           prefix.words = prefix.words,
                           order.maximum = 3L, discount = 0.4)

# Profiling message
# message(Sys.time(), " prediction complete, ngram.object is ",
#         format(object.size(ngram.table), units = "Mb"))

# Print prediction output if desired
# print(prediction)

