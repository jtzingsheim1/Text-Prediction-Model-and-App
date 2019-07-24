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
library(data.table)
source("capstone_functions.R")


# Part 1)-----------------------------------------------------------------------

# Read in text, convert to corpus, and then convert to tokens
ngrams <- AssembleCorpus(n.lines = 1000) %>%
  TokenizeAndClean(n = 1:3)

# Convert tokens object to table and perform calculations
ngrams <- ngrams %>%
  dfm() %>%
  textstat_frequency() %>%
  as_tibble() %>%
  select(feature, frequency) %>%
  mutate(frequency = as.integer(frequency)) %>%
  mutate(n = CountGrams(feature))
  # filter(frequency > 1) %>%
  # ApplyAlphaWords() %>% 
  # ApplyWCAttribute() %>%
  # ApplySbo1()

# Set the discount value to use
alpha.value <- 0.4

# This is the first step that requires discount values
# This is the first step that requires knowledge of the preceding word(s)
text.input <- "the oil"
prefix.words <- str_split(text.input, pattern = " ") %>%
  unlist()
# predictions <- MakePrediction(ngrams, preceding.words = previous.words,
#                               discount = alpha.value)
# print(predictions)

# ngram.table <- ngrams

# MakePrediction <- function(ngram.table, prefix.words) {
#   
#   message(Sys.time(), " making prediction")
#   
#   # Set how many words should be offered as predictions
#   predictions.needed <- 3L
#   order.maximum <- 2L
#   
#   FindWords(ngram.table, prefix.words, order.maximum, predictions.needed)
#   
# }

FindWords <- function(ngram.table, prefix.words, discount, order.maximum) {
  
  predictions.desired <- 5L
  predictions.found <- 0L
  
  # Create empty tables to be updated later
  pred.table.upper <- tibble(feature = character(0), word = character(0),
                             score = numeric(0))
  pred.table.lower <- pred.table.upper

  # Truncate input text to maximum length supported by the model
  prefix.words <- tail(prefix.words, order.maximum)
  order.used <- length(prefix.words)
  prefix.gram <- paste(prefix.words, collapse = "_")  # Convert to gram format
  
  # Check if preceding gram has been observed
  ngram.vector <- ngram.table %>%
    filter(n == order.used) %$%
    feature
  
  if (prefix.gram %in% ngram.vector) {
    
    message(Sys.time(), " preceding ", order.used, " gram observed ",
            "finding suffix words")
    
    n1gram.vector <- ngram.table %>%
      filter(n == order.used + 1L) %$%
      feature
    
    completing.grams <- GetCompletingGrams(prefix.gram = prefix.gram,
                                    n1gram.vector = n1gram.vector)
    
    completing.words <- ExtractWords(prefix.gram = prefix.gram,
                                     n1gram.vector = completing.grams)
    
    pred.table.upper <- ngram.table %>%
      filter(feature %in% completing.grams) %>%
      mutate(word = completing.words) %>%
      arrange(desc(frequency)) %>%
      ApplyScores() %>%
      slice(n = 1:predictions.desired) %>%
      select(feature, word, score)
    
    number.found <- length(completing.grams)

  }
  
  if (number.found < predictions.desired) {
    
    order.maximum <- order.maximum - 1L
    
    pred.table.lower <- FindWords(ngram.table, prefix.words, discount,
                                  order.maximum)
    pred.table.lower <- mutate(pred.table.lower, score = score * discount)
    
  }
  
  prediction.table <- bind_rows(pred.table.upper, pred.table.lower) %>%
    arrange(desc(score)) %>%
    distinct(word, .keep_all = TRUE) %>%
    slice(n = 1:predictions.desired)
  
  return(prediction.table)
  
}

prediction <- FindWords(ngrams, prefix.words, alpha.value, 2L)
print(prediction)

# need to do something about duplicate words
# is it possible for the algorithm to return less than the desired # of preds
# Should store order in the table object
# should retrieve the order instead of specifying it




