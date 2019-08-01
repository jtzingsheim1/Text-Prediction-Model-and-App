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
# prefix.words <- PrepareInputText("Where in the world is")

# Profiling message
# message(Sys.time(), " begin predicting words")

# Make prediction
# prediction <- PredictWords(ngram.table = ngram.table,
#                            prefix.words = prefix.words,
#                            order.maximum = 3L, discount = 0.4)

# Profiling message
# message(Sys.time(), " prediction complete, ngram.object is ",
#         format(object.size(ngram.table), units = "Mb"))

# Print prediction output if desired
# print(prediction)

# Measuring Accuracy of the Model ----------------------------------------------

# Build a function to get the top word from prediction results 
PredictWords2 <- function(prefix.words, ngram.table) {
  
  prediction.table <- PredictWords(ngram.table, prefix.words)
  
  best.word <- prediction.table %>%
    slice(n = 1) %$%
    word
  
  return(best.word)
  
}

# Build function to select random section of text
SplitTexts <- function(results.table) {
  
  prepared.texts <- results.table$prepared.text
  word.counts <- results.table$word.count
  
  inputs <- list(length(prepared.texts))
  answers <- character(length(prepared.texts))
  # randoms <- integer(length = length(word.counts))
  for (i in seq_along(word.counts)) {
    
    current.length <- word.counts[[i]]
    
    random <- sample(1:current.length, 1)
    excerpt <- head(prepared.texts[[i]], random)
    less.one <- random - 1
    inputs[[i]] <- head(excerpt, less.one)
    answers[[i]] <- tail(excerpt, 1)
    
  }
  
  results.table <- results.table %>%
    mutate(input.text = inputs) %>%
    mutate(answer = answers)
  
  return(results.table)
  
}

# Load data
text.blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8")
text.twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8",
                          warn = FALSE)

# Extract text from end (unused previously)
amount.topull <- 50L  # From each document, subset from bottom
testing.texts <- tail(text.blogs, amount.topull) %>%
  c(tail(text.twitter, amount.topull))

# Subset from top
prepared.texts <- map(testing.texts[1:10], PrepareInputText)
word.counts <- map_int(prepared.texts, length)
# split.position <- MakeRandoms(word.counts)

results.table <- tibble(prepared.text = prepared.texts,
                        word.count = word.counts) %>%
  SplitTexts()

# Make predictions
predicted.words <- map_chr(results.table$input.text, PredictWords2,
                           ngram.table = ngram.table)

results.table <- mutate(results.table, predicted.answer = predicted.words)

print(results.table)



