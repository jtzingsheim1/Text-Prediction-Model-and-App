
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
  
  message(Sys.time(), " assembling corpus")
  
  # Check and set arguments
  sub.dir <- match.arg(sub.dir)
  
  # Download and unzip the data, store folder name and file paths
  data.folder <- file.path(DLAndUnzipData(), sub.dir)
  file.names <- list.files(data.folder)  # Collect files names
  file.paths <- file.path(data.folder, file.names)  # Append file names to path
  
  # Read in the data and combine into a single corpus
  text.list <- map(file.paths, readLines, n = n.lines, encoding = "UTF-8",
                   warn = FALSE)
  blogs.corp <- corpus(text.list[[1]])
  news.corp <- corpus(text.list[[2]])
  twitter.corp <- corpus(text.list[[3]])
  full.corpus <- blogs.corp + news.corp + twitter.corp
  
  return(full.corpus)
}

TokenizeAndClean <- function(corpus, n = 1:2) {
  
  # message(Sys.time(), " tokenizing and cleaning text")
  message(Sys.time(), " tokenizing text with n = 1")
  
  # Build tokens object of unigrams
  tokens.object <- tokens(corpus, what = "fasterword", remove_numbers = TRUE,
                          remove_punct = TRUE, remove_symbols = TRUE,
                          remove_twitter = TRUE, remove_hyphens = TRUE,
                          remove_url = TRUE, ngrams = 1, verbose = FALSE)
  
  message(Sys.time(), " removing profanities")
  
  # Remove profanity from unigrams
  profanities <- readLines("profanity_list.txt")
  tokens.object <- tokens_remove(tokens.object, pattern = profanities,
                                 padding = TRUE)
  
  message(Sys.time(), " retokenizing text to n = 1:", max(n))
  
  # Convert tokens object to include bigrams and trigrams
  tokens.object <- tokens(tokens.object, ngrams = n)
  
  message(Sys.time(), " tokens object is ", format(object.size(tokens.object),
                                                   units = "Mb"))

  return(tokens.object)
  
}

CountGrams <- function(ngram.vector) {
  
  message(Sys.time(), " counting grams")
  
  ngram.n0 <- ngram.vector %>%
    str_extract_all("_") %>%
    map_int(length)
  
  ngram.n <- as.integer(ngram.n0 + 1)
  
  return(ngram.n)
  
}

GetDataFrom <- function(method = c("scratch", "saved.object"), n.lines = -1L,
                        n.max = 5L, min.occurances = 1L,
                        file.name = "ngram_table.Rdata") {
  
  method <- match.arg(method)

  if (method == "saved.object") {
    
    message(Sys.time(), " loading ngram table object from disk")
    
    # Code to load object from disk instead
    load(file.name)    
    
  } else {
    
    message(Sys.time(), " loading data from scratch with n.lines = ", n.lines)
    
    # Read in text, convert to corpus, and then convert to tokens
    ngram.object <- AssembleCorpus(n.lines = n.lines) %>%
      TokenizeAndClean(n = 1:n.max)
    
    # Convert tokens object to table and perform calculations
    
    message(Sys.time(), " converting tokens to dfm, trimming low frequencies")
    ngram.object <- ngram.object %>%
      dfm() %>%
      dfm_trim(min_termfreq = min.occurances)

    message(Sys.time(), " converting dfm to frequency table")
    ngram.object <- ngram.object %>%
      colSums() %>%
      enframe(name = "feature", value = "frequency") %>%
      mutate(frequency = as.integer((frequency))) %>%
      mutate(n = CountGrams(feature))
    
    message(Sys.time(), " saving ngram.table object to disk")
    
    save(ngram.object, file = file.name)

  }
  
  return(ngram.object)
  
}

GetCompletingGrams <- function(prefix.gram, n1gram.vector) {
  prefix.pattern <- paste0("^", prefix.gram, "_")
  completing.grams <- prefix.pattern %>%
    grep(n1gram.vector, value = TRUE, useBytes = TRUE)

  return(completing.grams)
}

ApplyScores <- function(ngram.table) {

  # message(Sys.time(), " calculating sbo scores and updating table")

  grams.count <- ngram.table %$%
    frequency %>%
    sum()
  ngram.table <- mutate(ngram.table, score = frequency / grams.count)

  return(ngram.table)

}

ExtractWords <- function(prefix.gram, n1gram.vector) {
  
  # message(Sys.time(), " extracting words from grams")
  
  prefix.pattern <- paste0(prefix.gram, "_")
  
  completing.words <- sub(prefix.pattern, "", n1gram.vector, fixed = TRUE,
                          useBytes = TRUE)
  
  return(completing.words)
  
}

PredictWords <- function(ngram.table, prefix.words, order.maximum = 4L,
                         discount = 0.4) {
  
  # message(Sys.time(), " begin predicting words function")
  
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
            "looking for suffix words")
    
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
    
    predictions.found <- length(completing.grams)
    
  }
  
  if (predictions.found < predictions.desired) {
    
    message(Sys.time(), " insufficient quantity of words, continue search")
    
    order.maximum <- order.maximum - 1L
    
    # Need if statement to prevent an infinite loop
    if (order.maximum > 0) {
      
      pred.table.lower <- PredictWords(ngram.table = ngram.table,
                                       prefix.words = prefix.words,
                                       order.maximum = order.maximum,
                                       discount = discount)
      pred.table.lower <- mutate(pred.table.lower, score = score * discount)
      
    } else {
      
      # Default to unigram frequency if insufficient bigram count
      pred.table.lower <- ngram.table %>%
        filter(n == 1) %>%
        mutate(word = feature) %>%
        arrange(desc(frequency)) %>%
        ApplyScores() %>%
        slice(n = 1:predictions.desired) %>%
        select(feature, word, score) %>%
        mutate(score = score * discount)
      
    }
    

    
  }
  
  prediction.table <- bind_rows(pred.table.upper, pred.table.lower) %>%
    arrange(desc(score)) %>%
    distinct(word, .keep_all = TRUE) %>%
    slice(n = 1:predictions.desired)
  
  return(prediction.table)
  
}

