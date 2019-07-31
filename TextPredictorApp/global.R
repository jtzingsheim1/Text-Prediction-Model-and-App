# Coursera Data Science Specialization Capstone Project Function Definitions----
# Predicting the next word in a sequence


# The purpose of this file is to define the functions which may be called by
# script or app for the project.
#
# The functions facilitate the project objectives of:
# - Analyze a large corpus of text documents
# - Discover structure in the data and how words are put together
# - Clean and analyze the text data
# - Build a predictive text model
# - Build a predictive text product that uses the model


# Function Definitions ---------------------------------------------------------

DLAndUnzipData <- function(profanity.filename = "profanity_list.txt",
                           data.filename = "Coursera-SwiftKey.zip") {
  # Downloads and unzips the data if needed, returns dataset folder name
  #
  # Args:
  #   profanity.filename: An optional alternative name for the profanity file
  #   data.filename: An optional alternative name for the zip file
  #
  # Returns:
  #   A character value of the name of the folder containing the data
  
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
  
  # Check if the data file is already unzipped, unzip if needed
  data.folder <- "final"
  if (!file.exists(data.folder)) {
    message("Unzipping data file")
    unzip(data.filename)
  }
  
  # Return the directory name for the unzipped data
  return(data.folder)
  
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
  
  # Profiling message
  message(Sys.time(), " assembling corpus")
  
  # Check and set input arguments
  sub.dir <- match.arg(sub.dir)
  
  # Download and unzip the data, store folder name and file paths
  data.folder <- file.path(DLAndUnzipData(), sub.dir)
  file.names <- list.files(data.folder)
  file.paths <- file.path(data.folder, file.names)
  
  # Read in the data and combine into a single corpus
  text.list <- map(file.paths, readLines, n = n.lines, encoding = "UTF-8",
                   warn = FALSE)
  blogs.corp <- corpus(text.list[[1]])
  news.corp <- corpus(text.list[[2]])
  twitter.corp <- corpus(text.list[[3]])
  full.corpus <- blogs.corp + news.corp + twitter.corp
  
  # Return a single corpus containing text from all the files
  return(full.corpus)
  
}

TokenizeAndClean <- function(corpus, n = 1:2) {
  # Tokenizes to indicated n and cleans, including profanity removal
  #
  # Args:
  #   corpus: a corpus object from the quanteda package
  #   n: the level of ngrams desired, can be a single value or a sequence
  #
  # Returns:
  #   A tokens object which has been preprocessed and cleaned
  
  # Profiling message
  message(Sys.time(), " tokenizing text with n = 1")
  
  # Build tokens object of unigrams only, some cleaning applied
  tokens.object <- tokens(corpus, what = "word", remove_numbers = TRUE,
                          remove_punct = TRUE, remove_symbols = TRUE,
                          remove_twitter = TRUE, remove_hyphens = TRUE,
                          remove_url = TRUE, ngrams = 1, verbose = FALSE)
  
  # Profiling message
  message(Sys.time(), " removing profanities")
  
  # Remove profanity from unigrams, pad to preserve structure
  profanities <- readLines("profanity_list.txt")
  tokens.object <- tokens_remove(tokens.object, pattern = profanities,
                                 padding = TRUE)
  
  # Profiling message
  message(Sys.time(), " retokenizing text to n = 1:", max(n))
  
  # Convert tokens object to include additional specified ngrams
  tokens.object <- tokens(tokens.object, ngrams = n)
  
  # Profiling message
  message(Sys.time(), " tokens object is ", format(object.size(tokens.object),
                                                   units = "Mb"))
  
  # Return the tokens object
  return(tokens.object)
  
}

CountGrams <- function(ngram.vector) {
  # Counts the number of underscore characters in each string of a vector
  #
  # Args:
  #   ngram.vector: a vector of ngrams
  #
  # Returns:
  #   A vector of integers representing the number of underscores in each gram
  
  # Profiling message
  message(Sys.time(), " counting grams")
  
  # Count the number of underscores for each element of the vector
  ngram.n0 <- ngram.vector %>%
    str_extract_all("_") %>%
    map_int(length)
  
  # Add one to each
  ngram.n <- as.integer(ngram.n0 + 1)
  
  # Return the counts
  return(ngram.n)
  
}

GetDataFrom <- function(method = c("scratch", "saved.object"),
                        file.name = "ngram_table.Rdata", n.lines = -1L,
                        n.max = 4L, min.occurances = 1L) {
  # Gets an ngram table object from scratch or from disk
  #
  # Args:
  #   method: indicates where to get the data from: scratch or disk
  #   file.name: the name of the data object to load or save
  #   n.lines: number of lines to read in if data is collected from scratch
  #   n.max: the maximum ngram to create if data is from scratch
  #   min.occurances: minimum frequency needed to include an ngram in the result
  #
  # Returns:
  #   A three column tibble of features, their frequencies, and their order (n)
  
  # Check and set input arguments
  method <- match.arg(method)
  
  # Check if the object should be loaded from disk
  if (method == "saved.object") {
    
    # Profiling message
    message(Sys.time(), " loading ngram table object from disk")
    
    # Load object from disk
    load(file.name)    
    
  } else {
    
    # Profiling message
    message(Sys.time(), " loading data from scratch with n.lines = ", n.lines)
    
    # Read in text, convert to corpus, and then convert to tokens
    ngram.object <- AssembleCorpus(n.lines = n.lines) %>%
      TokenizeAndClean(n = 1:n.max)
    
    # Profiling message
    message(Sys.time(), " converting tokens to dfm, trimming low frequencies")
    
    # Convert tokens object to table and remove features with insufficient freq
    ngram.object <- ngram.object %>%
      dfm() %>%
      dfm_trim(min_termfreq = min.occurances)
    
    # Profiling message
    message(Sys.time(), " converting dfm to frequency table")
    
    # Convert dfm to frequency table as tibble object
    ngram.object <- ngram.object %>%
      colSums() %>%
      enframe(name = "feature", value = "frequency") %>%
      mutate(frequency = as.integer((frequency))) %>%
      mutate(n = CountGrams(feature))
    
    # Profiling message
    message(Sys.time(), " saving ngram.table object to disk")
    
    # Save object to disk
    save(ngram.object, file = file.name)

  }
  
  # Return the ngam object as a two column tibble
  return(ngram.object)
  
}

PrepareInputText <- function(input.string) {
  # Prepares text input for prediction model, removes punctuation and uppercase
  #
  # Args:
  #   input.string: a single string of text to process
  #
  # Returns:
  #   A character vector, one word each, lower case, punctuation removed
  
  # Process input text for prediction model
  prefix.words <- input.string %>%
    gsub(pattern = '[[:punct:]]', replacement = "", .) %>%
    tolower() %>%
    str_split(pattern = " ") %>%
    unlist()
  
  # Return processed text as a character vector
  return(prefix.words)
  
}

GetCompletingGrams <- function(prefix.gram, n1gram.vector) {
  # Finds which grams complete a given input gram
  #
  # Args:
  #   prefix.gram: the ngram prefix to look for, order of n-1
  #   n1gram.vector: a vector of observed ngrams, order of n
  #
  # Returns:
  #   A character vector of ngrams that were observed
  
  # Prepare the prefix for search
  prefix.pattern <- paste0("^", prefix.gram, "_")
  
  # Find the pattern in the n1 vector
  completing.grams <- prefix.pattern %>%
    grep(n1gram.vector, value = TRUE, useBytes = TRUE)
  
  # Return the observed ngrams as a character vector
  return(completing.grams)
  
}

ExtractWords <- function(prefix.gram, n1gram.vector) {
  # Extracts the last word from a vector of ngrams by removing the prefix
  #
  # Args:
  #   prefix.gram: the prefix, of order n-1, to remove from the vector
  #   n1gram.vector: the vector of order n, from which to remove prefixes
  #
  # Returns:
  #   A character vector of words that completed the input prefix.gram
  
  # Prepare prefix for search
  prefix.pattern <- paste0(prefix.gram, "_")
  
  # Remove the pattern from the n1 vector
  completing.words <- sub(prefix.pattern, "", n1gram.vector, fixed = TRUE,
                          useBytes = TRUE)
  
  # Return the observed words as a character vector
  return(completing.words)
  
}

ApplyScores <- function(ngram.table) {
  # Calculates a stupid backoff score for observed ngrams, must be pre-filtered
  #
  # Args:
  #   ngram.table: a table of ngrams and their frequencies
  #
  # Returns:
  #   A table with a column added for the frequency score of each gram
  
  # Calculate sum of observed frequencies
  grams.count <- ngram.table %$%
    frequency %>%
    sum()
  
  # Calculate the score and add it as a column
  ngram.table <- mutate(ngram.table, score = frequency / grams.count)
  
  # Return the updated ngram tibble object
  return(ngram.table)
  
}

PredictWords <- function(ngram.table, prefix.words, order.maximum = 3L,
                         discount = 0.4) {
  # Prepares word predictions using a modified stupid backoff method
  #
  # Args:
  #   ngram.table: a full ngram table with features and frequencies
  #   prefix.words: a character vector of pre-processed text to predict from
  #   order.maximum: n where search should start (ex 3 takes trigram input text)
  #   discount: the backoff discount to apply with each recursion
  #
  # Returns:
  #   A three column tibble of predicted words, scores, and features
  
  # Specify how many predictions to find and set current value to zero
  predictions.desired <- 5L
  predictions.found <- 0L
  
  # Create empty tables to be updated after recursion
  pred.table.upper <- tibble(word = character(0), score = numeric(0),
                             feature = character(0))
  pred.table.lower <- pred.table.upper
  
  # Truncate input text to maximum length supported by the model/set by user
  prefix.words <- tail(prefix.words, order.maximum)
  
  # Check order used, could be less than max if user supplied less input
  order.used <- length(prefix.words)
  
  # Convert input text to gram format by adding underscores between words
  prefix.gram <- paste(prefix.words, collapse = "_")
  
  # Prepare a search vector of grams where the order matches the input gram
  ngram.vector <- ngram.table %>%
    filter(n == order.used) %$%
    feature
  
  # Check if input gram was observed in training data
  if (prefix.gram %in% ngram.vector) {
    
    # If input gram was observed prepare a vector of grams of order n+1
    n1gram.vector <- ngram.table %>%
      filter(n == order.used + 1L) %$%
      feature
    
    # Find the grams which represent completions of the input gram
    completing.grams <- GetCompletingGrams(prefix.gram = prefix.gram,
                                           n1gram.vector = n1gram.vector)
    
    # Extract the last words from the matching grams
    completing.words <- ExtractWords(prefix.gram = prefix.gram,
                                     n1gram.vector = completing.grams)
    
    # Prepare the results as a table
    pred.table.upper <- ngram.table %>%
      filter(feature %in% completing.grams) %>%  # Filter table to the matches
      mutate(word = completing.words) %>%  # Add column of completing words
      arrange(desc(frequency)) %>%
      ApplyScores() %>%  # Calculate scores for each feature
      slice(n = 1:predictions.desired) %>%  # Take the top predictions
      select(word, score, feature)
    
    # Count how many matching words found in this search, not a running total
    predictions.found <- length(completing.grams)
    
  }
  
  # Decide if search will continue to recurse through smaller order grams
  if (predictions.found < predictions.desired) {
    
    # Decrement the maximum order to apply search to the next lower order
    order.maximum <- order.maximum - 1L
    
    # Check if current order is for unigrams, prevents infinite recursion
    if (order.maximum > 0) {
      
      # Function calls itself for the next lower order
      pred.table.lower <- PredictWords(ngram.table = ngram.table,
                                       prefix.words = prefix.words,
                                       order.maximum = order.maximum,
                                       discount = discount)
      
      # Reduce the score by multiplying the discount factor
      pred.table.lower <- mutate(pred.table.lower, score = score * discount)
    
    # If current order is for unigrams, stop recursion and calculate directly  
    } else {
      
      # Prepare the results as a table
      pred.table.lower <- ngram.table %>%
        filter(n == 1) %>%  # Extract all unigrams
        mutate(word = feature) %>%
        arrange(desc(frequency)) %>%
        ApplyScores() %>%  # Calculate frequency scores
        slice(n = 1:predictions.desired) %>%  # Take the top predictions
        select(word, score, feature) %>%
        mutate(score = score * discount)  # Apply discount
      
    }
    
  }
  
  # Once either or both tables are populated, bind them together
  prediction.table <- bind_rows(pred.table.upper, pred.table.lower) %>%
    arrange(desc(score)) %>%
    distinct(word, .keep_all = TRUE) %>%  # Remove duplicate predictions
    slice(n = 1:predictions.desired)  # Take the top predictions
  
  # Return the prediction table result
  return(prediction.table)
  
}

