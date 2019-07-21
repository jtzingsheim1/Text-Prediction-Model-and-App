
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
  text.list <- map(file.paths, readLines, n = n.lines)
  blogs.corp <- corpus(text.list[[1]])
  news.corp <- corpus(text.list[[2]])
  twitter.corp <- corpus(text.list[[3]])
  full.corpus <- blogs.corp + news.corp + twitter.corp
  
  return(full.corpus)
}

TokenizeAndClean <- function(corpus) {
  
  message(Sys.time(), " tokenizing and cleaning text")
  # message(Sys.time(), " tokenizing text with n = 1")
  
  # Build tokens object of unigrams
  tokens.object <- tokens(corpus, what = "word", remove_numbers = TRUE,
                          remove_punct = TRUE, remove_symbols = TRUE,
                          remove_twitter = TRUE, remove_hyphens = TRUE,
                          remove_url = TRUE, ngrams = 1, verbose = FALSE)
  
  # message(Sys.time(), " removing profanities")
  
  # Remove profanity from unigrams
  profanities <- readLines("profanity_list.txt")
  tokens.object <- tokens_remove(tokens.object, pattern = profanities,
                                 padding = TRUE)
  
  # message(Sys.time(), " retokenizing text to n = 1:3")
  
  # Convert tokens object to include bigrams and trigrams
  tokens.object <- tokens(tokens.object, n = 1:3)
  # Testing shows that this sequence takes about the same time as tokenizing to
  # 1:3 initially
  
  return(tokens.object)
  
}

CountGrams <- function(ngram.vector) {
  
  # message(Sys.time(), " counting grams")
  
  ngram.n0 <- ngram.vector %>%
    str_extract_all("_") %>%
    map_int(length)
  
  ngram.n <- as.integer(ngram.n0 + 1)
  
  return(ngram.n)
  
}

RemoveSingletons <- function(ngram.table) {
  
  # message(Sys.time(), " removing singletons")
  
  # Preserve all unigrams
  unigram.portion <- filter(ngram.table, n == 1)
  
  # Eliminate non-unigrams with frequency of 1
  other.portion <- ngram.table %>%
    filter(n != 1) %>%
    filter(frequency > 1)
  
  ngram.table <- bind_rows(unigram.portion, other.portion)
  
  return(ngram.table)
  
}

ApplyWCAttribute <- function(ngram.table) {
  
  # message(Sys.time(), " applying word count attribute")
  
  attr(ngram.table, "word.count") <- ngram.table %>%
    filter(n == 1) %$%
    frequency %>%
    sum()
  
  return(ngram.table)
  
}

ApplyQMLs <- function(ngram.table) {
  
  message(Sys.time(), " calculating qmls and updating table")
  
  word.count <- ngram.table %>%
    attributes() %$%
    word.count
  unigram.portion <- ngram.table %>%
    filter(n == 1) %>%
    mutate(qml = frequency / word.count)
  other.portion <- ngram.table %>%
    filter(n != 1) %>%
    mutate(qml = NA_real_)
  
  ngram.table <- bind_rows(unigram.portion, other.portion)
  
  return(ngram.table)
  
}

ApplyAdjFreq <- function(ngram.table, discount.bis, discount.tris) {
  
  message(Sys.time(), " calculating adjusted frequencies and updating table")
  
  unigrams <- ngram.table %>%
    filter(n == 1) %>%
    mutate(adj.freq = NA_real_)
  bigrams <- ngram.table %>%
    filter(n == 2) %>%
    mutate(adj.freq = frequency - discount.bis)
  trigrams <- ngram.table %>%
    filter(n == 3) %>%
    mutate(adj.freq = frequency - discount.tris)
  
  ngram.table <- bind_rows(unigrams, bigrams, trigrams)
  
  return(ngram.table)
  
}

GetAlphaWords <- function(prefix.ngram, n1gram.vector) {
  prefix.pattern <- paste0(prefix.ngram, "_")
  observed.suffixes <- prefix.pattern %>%
    grep(n1gram.vector, value = TRUE, fixed = TRUE, useBytes = TRUE) %>%
    sub(prefix.pattern, "", .)
  
  # Test if vector is empty, and replace with NA if TRUE
  if (length(observed.suffixes) == 0) {
    observed.suffixes <- NA_character_
  }
  
  return(observed.suffixes)
}

GetBetaValues <- function(alpha.words.element, unigram.table,
                          value.type = c("qml", "qbo.bi")) {
  
  # Match value type and check alpha words for NAs
  value.type <- match.arg(value.type)
  na.count <- sum(is.na(alpha.words.element))

  # Define the beta words
  if (na.count != 0) {
    alpha.sum <- 0
  } else {
    alpha.sum <- unigram.table %>%
      filter(feature %in% alpha.words.element) %>%
      .[[value.type]] %>%
      sum()
  }

  # Extract and sum the desired values
  beta.sum <- 1 - alpha.sum

  return(beta.sum)

}

# GetQboSums <- function(alpha.words.element, unigram.table) {
#   
#   na.count <- sum(is.na(alpha.words.element))
#   
#   # Define the beta words
#   if (na.count != 0) {
#     alpha.sum <- 0
#   } else {
#     alpha.sum <- unigram.table %>%
#       filter(feature %in% alpha.words) %>%
#       
#       tokens_remove(pattern = alpha.words.element) %>%
#       as.character()
#   }
#   
#   # Extract and sum the desired values
#   betas.sum <- unigram.table %>%
#     filter(feature %in% beta.words) %>%
#     .[[value.type]] %>%
#     sum()
#   
#   return(betas.sum)
#   
# }

# GetBetaValues <- function(alpha.words.element, vocab.tokens, unigram.table,
#                           value.type = c("qml", "qbo.bi")) {
# 
#   # Match value type and check alpha words for NAs
#   value.type <- match.arg(value.type)
#   na.count <- sum(is.na(alpha.words.element))
# 
#   # Define the beta words
#   if (na.count != 0) {
#     beta.words <- as.character(vocab.tokens)
#   } else {
#     beta.words <- vocab.tokens %>%
#       tokens_remove(pattern = alpha.words.element) %>%
#       as.character()
#   }
# 
#   # Extract and sum the desired values
#   betas.sum <- unigram.table %>%
#     filter(feature %in% beta.words) %>%
#     .[[value.type]] %>%
#     sum()
# 
#   return(betas.sum)
# 
# }

# ApplyAlphaWords <- function (ngram.table) {
#   
#   message(Sys.time(), " finding alpha words and updating tables")
#   
#   unigrams <- filter(ngram.table, n == 1)
#   bigrams <- filter(ngram.table, n == 2)
#   trigrams <- filter(ngram.table, n == 3)
#   
#   uni.alpha.words <- map(unigrams$feature, GetAlphaWords,
#                          n1gram.vector = bigrams$feature)
#   unigrams <- mutate(unigrams, alpha.words = uni.alpha.words)
# 
#   bi.alpha.words <- map(bigrams$feature, GetAlphaWords,
#                         n1gram.vector = trigrams$feature)
#   bigrams <- mutate(bigrams, alpha.words = bi.alpha.words)
#   
#   trigrams <- mutate(trigrams, alpha.words = list(NA_character_))
# 
#   ngram.table <- bind_rows(unigrams, bigrams, trigrams)
#   
#   return(ngram.table)
#   
# }

# ApplySomethingElse <- function (ngram.table) {
# 
#   unigrams <- filter(ngram.table, n == 1)
#   bigrams <- filter(ngram.table, n == 2)
#   trigrams <- filter(ngram.table, n == 3)
# 
#   # message(Sys.time(), " finding unigram alpha words")
#   # 
#   # vocabulary.tokens <- tokens(unigrams$feature, n = 1)
#   # uni.alpha.words <- map(unigrams$feature, GetAlphaWords,
#   #                   n1gram.vector = bigrams$feature)
#   # 
#   # message(Sys.time(), " calculating unigram beta values")
# 
#   uni.beta.vals <- map_dbl(uni.alpha.words, GetBetaValues,
#                            vocab.tokens = vocabulary.tokens,
#                            unigram.table = unigrams, value.type = "qml")
# 
#   unigrams <- unigrams %>%
#     mutate(alpha.words = uni.alpha.words) %>%
#     mutate(qml.sum = uni.beta.vals)
# 
#   message(Sys.time(), " calculating bigram alpha words")
# 
#   bi.alpha.words <- map(bigrams$feature, GetAlphaWords,
#                         n1gram.vector = trigrams$feature)
# 
#   bigrams <- bigrams %>%
#     mutate(alpha.words = bi.alpha.words) %>%
#     mutate(qml.sum = NA_real_)
# 
#   trigrams <- trigrams %>%
#     mutate(alpha.words = list(NA_character_)) %>%
#     mutate(qml.sum = NA_real_)
# 
#   ngram.table <- bind_rows(unigrams, bigrams, trigrams)
# 
#   return(ngram.table)
# 
# }

ApplyAlphaWords <- function (ngram.table) {

  unigrams <- filter(ngram.table, n == 1)
  bigrams <- filter(ngram.table, n == 2)
  trigrams <- filter(ngram.table, n == 3)

  message(Sys.time(), " finding unigram alpha words")

  #vocabulary.tokens <- tokens(unigrams$feature, n = 1)
  uni.alpha.words <- map(unigrams$feature, GetAlphaWords,
                    n1gram.vector = bigrams$feature)

  message(Sys.time(), " calculating unigram beta values")

  uni.beta.vals <- map_dbl(uni.alpha.words, GetBetaValues,
                           #vocab.tokens = vocabulary.tokens,
                           unigram.table = unigrams, value.type = "qml")

  unigrams <- unigrams %>%
    mutate(alpha.words = uni.alpha.words) %>%
    mutate(qml.sum = uni.beta.vals)

  message(Sys.time(), " calculating bigram alpha words")

  bi.alpha.words <- map(bigrams$feature, GetAlphaWords,
                        n1gram.vector = trigrams$feature)

  bigrams <- bigrams %>%
    mutate(alpha.words = bi.alpha.words) %>%
    mutate(qml.sum = NA_real_)

  trigrams <- trigrams %>%
    mutate(alpha.words = list(NA_character_)) %>%
    mutate(qml.sum = NA_real_)

  ngram.table <- bind_rows(unigrams, bigrams, trigrams)

  return(ngram.table)

}

GetAlphaValues <- function(prefix.ngram, ngram.table, n1gram.table, discount) {
  observed.suffixes <- ngram.table %>%
    filter(feature == prefix.ngram) %$%
    alpha.words %>%
    unlist()
  
  na.count <- sum(is.na(observed.suffixes))
  
  if (na.count != 0) {
    alpha.sums <- NA_real_
  } else {
    # Assemble matching n1grams
    matching.n1grams <- paste(prefix.ngram, observed.suffixes, sep = "_")
    
    freq.vector <- n1gram.table %>%
      filter(feature %in% matching.n1grams) %$%
      frequency
    
    alpha.sums <- discount * length(freq.vector) / sum(freq.vector)
    
  }
  
  return(alpha.sums)
  
}

ApplyAlphaValues <- function(ngram.table, discount.bis, discount.tris) {
  
  message(Sys.time(), " calculating alpha values and updating table")
  
  unigrams <- filter(ngram.table, n == 1)
  bigrams <- filter(ngram.table, n == 2)
  trigrams <- filter(ngram.table, n == 3)
  
  uni.alpha.vals <- map_dbl(unigrams$feature, GetAlphaValues,
                            ngram.table = unigrams, n1gram.table = bigrams,
                            discount = discount.bis)
  bi.alpha.vals <- map_dbl(bigrams$feature, GetAlphaValues,
                           ngram.table = bigrams, n1gram.table = trigrams,
                           discount = discount.tris)
  
  unigrams <- mutate(unigrams, alpha.value = uni.alpha.vals)
  bigrams <- mutate(bigrams, alpha.value = bi.alpha.vals)
  trigrams <- mutate(trigrams, alpha.value = NA_real_)
  
  ngram.table <- bind_rows(unigrams, bigrams, trigrams)
  
  return(ngram.table)  
  
}

GetQboBigram <- function(word, preceding.word, unigram.table, bigram.table) {
  
  alpha.words.v <- unigram.table %>%
    filter(feature == preceding.word) %$%
    alpha.words %>%
    unlist()

  alpha.test <- word %in% alpha.words.v

  if (alpha.test) {
    bigram <- paste(preceding.word, word, sep = "_")
    big.adj.freq <- bigram.table %>%
      filter(feature == bigram) %$%
      adj.freq
    uni.freq.v <- unigram.table %>%
      filter(feature == preceding.word) %$%
      frequency
    qbo.value.w <- big.adj.freq / uni.freq.v
  } else {
    alpha.value.v <- unigram.table %>%
      filter(feature == preceding.word) %$%
      alpha.value
    qml.w <- unigram.table %>%
      filter(feature == word) %$%
      qml
    qml.sum.v <- unigram.table %>%
      filter(feature == preceding.word) %$%
      qml.sum
    qbo.value.w <- alpha.value.v * qml.w / qml.sum.v
  }

  return(qbo.value.w)

}

GetQboTrigram <- function(word, preceding.words, unigram.table, bigram.table,
                       trigram.table) {
  
  bigram.pref <- paste(preceding.words, collapse = "_")
  alpha.words.uv <- bigram.table %>%
    filter(feature == bigram.pref) %$%
    alpha.words %>%
    unlist()
  
  alpha.test <- word %in% alpha.words.uv

  if (alpha.test) {
    trigram <- paste(bigram.pref, word, sep = "_")
    trig.adj.freq <- trigram.table %>%
      filter(feature == trigram) %$%
      adj.freq
    bi.freq.uv <- bigram.table %>%
      filter(feature == bigram.pref) %$%
      frequency
    qbo.value.w <- trig.adj.freq / bi.freq.uv
    
  } else {
    alpha.value.uv <- bigram.table %>%
      filter(feature == bigram.pref) %$%
      alpha.value
    qbo.w <- unigram.table %>%
      filter(feature == word) %$%
      qbo.bi
    qbo.sum.uv <- bigram.table %>%
      filter(feature == bigram.pref) %$%
      qbo.sum
    qbo.value.w <- alpha.value.uv * qbo.w / qbo.sum.uv
    
  }

  return(qbo.value.w)

}

# Need to update this function so it can handle variable input types for words
# MakePrediction <- function(ngram.table, preceding.words) {
#   
#   message(Sys.time(), " making prediction")
#   
#   unigrams <- filter(ngram.table, n == 1)
#   bigrams <- filter(ngram.table, n == 2)
#   trigrams <- filter(ngram.table, n == 3)
#   
#   # Test if preceding word (v) has been observed
#   v.test <- preceding.words[2] %in% unigrams$feature
#   
#   if (!v.test) {
#     
#     message(Sys.time(), " preceding word not observed, predicting from qml")
#     
#     prediction.table <- unigrams %>%
#       select(feature, qml) %>%
#       arrange(desc(qml))
#     
#   } else {
#     
#     # Create a tokens object from all observed unigrams
#     vocabulary.tokens <- tokens(unigrams$feature, n = 1)
#     
#     # Populate qbo values for unigrams
#     uni.qbo.bis <- map_dbl(unigrams$feature, GetQboBigram,
#                            preceding.word = preceding.words[2],
#                            unigram.table = unigrams, bigram.table = bigrams)
#     
#     unigrams <- mutate(unigrams, qbo.bi = uni.qbo.bis)
#     
#     # Test if preceding bigram (u, v) has been observed
#     bigram.pref <- paste(preceding.words, collapse = "_")
#     uv.test <- bigram.pref %in% bigrams$feature
#     
#     if (!uv.test) {
#       
#       message(Sys.time(), " preceding bigram not observed, predict from qbo.bi")
#       
#       prediction.table <- unigrams %>%
#         select(feature, qml, qbo.bi) %>%
#         arrange(desc(qbo.bi))
#       
#     } else {
#       
#       message(Sys.time(), " preceding bigram observed, predict from qbo.tri")
#       
#       bi.qbo.sums <- map_dbl(bigrams$alpha.words, GetBetaValues,
#                              vocab.tokens = vocabulary.tokens,
#                              unigram.table = unigrams, value.type = "qbo.bi")
#       
#       bigrams <- mutate(bigrams, qbo.sum = bi.qbo.sums)
# 
#       uni.qbo.tris <- map_dbl(unigrams$feature, GetQboTrigram,
#                               preceding.words = preceding.words,
#                               unigram.table = unigrams, bigram.table = bigrams,
#                               trigram.table = trigrams)
#       
#       prediction.table <- unigrams %>%
#         mutate(qbo.tri = uni.qbo.tris) %>%
#         select(feature, qml, qbo.bi, qbo.tri) %>%
#         arrange(desc(qbo.tri))
#       
#     }
#     
#   }
#   
#   message(Sys.time(), " prediction complete")
# 
#   return(prediction.table)
#   
# }

MakePrediction <- function(ngram.table, preceding.words) {
  
  message(Sys.time(), " making prediction")
  
  unigrams <- filter(ngram.table, n == 1)
  bigrams <- filter(ngram.table, n == 2)
  trigrams <- filter(ngram.table, n == 3)
  
  # Test if preceding word (v) has been observed
  v.test <- preceding.words[2] %in% unigrams$feature
  
  if (!v.test) {
    
    message(Sys.time(), " preceding word not observed, predict from qml")
    
  } else {
    
    # Create a tokens object from all observed unigrams
    #vocabulary.tokens <- tokens(unigrams$feature, n = 1)
    
    # Populate qbo values for unigrams
    uni.qbo.bis <- map_dbl(unigrams$feature, GetQboBigram,
                           preceding.word = preceding.words[2],
                           unigram.table = unigrams, bigram.table = bigrams)
    
    unigrams <- mutate(unigrams, qbo.bi = uni.qbo.bis)
    bigrams <- mutate(bigrams, qbo.bi = NA_real_)
    trigrams <- mutate(trigrams, qbo.bi = NA_real_)
    
    # Test if preceding bigram (u, v) has been observed
    bigram.pref <- paste(preceding.words, collapse = "_")
    uv.test <- bigram.pref %in% bigrams$feature
    
    if (!uv.test) {
      
      message(Sys.time(), " preceding bigram not observed, predict from qbo.bi")
      
    } else {
      
      message(Sys.time(), " preceding bigram observed, predict from qbo.tri")
      
      bi.qbo.sums <- map_dbl(bigrams$alpha.words, GetBetaValues,
                             #vocab.tokens = vocabulary.tokens,
                             unigram.table = unigrams, value.type = "qbo.bi")
      
      unigrams <- mutate(unigrams, qbo.sum = NA_real_)
      bigrams <- mutate(bigrams, qbo.sum = bi.qbo.sums)
      trigrams <- mutate(trigrams, qbo.sum = NA_real_)
      
      uni.qbo.tris <- map_dbl(unigrams$feature, GetQboTrigram,
                              preceding.words = preceding.words,
                              unigram.table = unigrams, bigram.table = bigrams,
                              trigram.table = trigrams)
      
      unigrams <- mutate(unigrams, qbo.tri = uni.qbo.tris)
      bigrams <- mutate(bigrams, qbo.tri = NA_real_)
      trigrams <- mutate(trigrams, qbo.tri = NA_real_)
      
    }
    
  }
  
  ngram.table <- bind_rows(unigrams, bigrams, trigrams)
  
  message(Sys.time(), " prediction complete")
  
  return(ngram.table)
  
}


