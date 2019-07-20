# Functions --------------------------------------------------------------------

CountGrams <- function(ngram.vector) {
  ngram.n0 <- ngram.vector %>%
    str_extract_all("_") %>%
    map_int(length)
  
  ngram.n <- as.integer(ngram.n0 + 1)
  
  return(ngram.n)
  
}

ApplyWCAttribute <- function(ngram.table) {
  attr(ngram.table, "word.count") <- ngram.table %>%
    filter(n == 1) %$%
    frequency %>%
    sum()
  
  return(ngram.table)
  
}

ApplyQMLs <- function(ngram.table) {
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

GetBetaValues <- function(alpha.words.element, vocab.tokens, unigram.table,
                          value.type = c("qml", "qbo")) {
  
  # Match value type and check alpha words for NAs
  value.type <- match.arg(value.type)
  na.count <- sum(is.na(alpha.words.element))
  
  # Define the beta words
  if (na.count != 0) {
    beta.words <- as.character(vocab.tokens)
  } else {
    beta.words <- vocab.tokens %>%
      tokens_remove(pattern = alpha.words.element) %>%
      as.character()
  }
  
  # Extract and sum the desired values
  betas.sum <- unigram.table %>%
    filter(feature %in% beta.words) %>%
    .[[value.type]] %>%
    sum()
  
  return(betas.sum)
  
}

ApplyAlphaWords <- function (ngram.table, discount.bis, discount.tris) {
  
  unigrams <- filter(ngram.table, n == 1)
  bigrams <- filter(ngram.table, n == 2)
  trigrams <- filter(ngram.table, n == 3)
  
  vocabulary.tokens <- tokens(unigrams$feature, n = 1)
  uni.alpha.words <- map(unigrams$feature, GetAlphaWords,
                    n1gram.vector = bigrams$feature)
  uni.beta.vals <- map_dbl(uni.alpha.words, GetBetaValues,
                           vocab.tokens = vocabulary.tokens,
                           unigram.table = unigrams, value.type = "qml")
  
  unigrams <- unigrams %>%
    mutate(alpha.words = uni.alpha.words) %>%
    mutate(qml.sum = uni.beta.vals)
  
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



# Script -----------------------------------------------------------------------

library(tidyverse)
library(quanteda)
library(magrittr)

corpus1 <- c("SOS buy the book EOS",
             "SOS buy the book EOS",
             "SOS buy the book EOS",
             "SOS buy the book EOS",
             "SOS sell the book EOS",
             "SOS buy the house EOS",
             "SOS buy the house EOS",
             "SOS paint the house EOS")

d2 <- 0.5
d3 <- 0.5

# unigram.prefix <- "the"
# bigram.prefix <- c("sell", "the")

# Build a single frequency table of 1:3 grams
ngrams <- corpus1 %>%
  tokens(n = 1:3) %>%
  dfm() %>%
  textstat_frequency() %>%
  as_tibble() %>%
  select(feature, frequency) %>%
  mutate(frequency = as.integer(frequency)) %>%
  mutate(n = CountGrams(feature)) %>%
  ApplyWCAttribute() %>%
  ApplyQMLs() %>%
  ApplyAdjFreq(discount.bis = d2, discount.tris = d3) %>%
  ApplyAlphaWords(discount.bis = d2, discount.tris = d3) %>%
  ApplyAlphaValues(discount.bis = d2, discount.tris = d3)


### Will need to find a way to remove profanity from this type of object












# This is the first step that requires knowledge of the preceding word
# QboBigram <- function(word, preceding.word, unigram.table, bigram.table) {
#   alpha.words1 <- unigram.table %>%
#     filter(feature == preceding.word) %$%
#     alpha.words %>%
#     unlist()
#   
#   alpha.test <- word %in% alpha.words1
#   
#   if (alpha.test) {
#     bigram <- paste(preceding.word, word, sep = "_")
#     big.adj.freq <- bigram.table %>%
#       filter(feature == bigram) %$%
#       adj.freq
#     uni.freq <- unigram.table %>%
#       filter(feature == preceding.word) %$%
#       frequency
#     qbo.value <- big.adj.freq / uni.freq
#   } else {
#     alpha.value <- unigram.table %>%
#       filter(feature == preceding.word) %$%
#       alpha.value
#     qml.w <- unigram.table %>%
#       filter(feature == word) %$%
#       qml
#     beta.q.sums.w <- unigram.table %>%
#       filter(feature == preceding.word) %$%
#       beta.q.sums
#     qbo.value <- alpha.value * qml.w / beta.q.sums.w
#   }
#   
#   return(qbo.value)
#   
# }
# 
# uni.qbos <- map_dbl(unigrams$feature, QboBigram,
#                     preceding.word = unigram.prefix, unigram.table = unigrams,
#                     bigram.table = bigrams)
# 
# unigrams <- unigrams %>%
#   mutate(qbos = uni.qbos)
# 
# 
# 
# beta.qbosums <- map_dbl(bigrams$beta.words, beta.qbosum,
#                         unigram.table = unigrams)
# 
# bigrams <- bigrams %>%
#   mutate(beta.qbo.sums = beta.qbosums)
# rm(uni.qbos, beta.qbosums)
# 
# QboTrigram <- function(word, preceding.words, unigram.table, bigram.table,
#                        trigram.table) {
#   bigram.pref <- paste(preceding.words, collapse = "_")
#   alpha.words2 <- bigram.table %>%
#     filter(feature == bigram.pref) %$%
#     alpha.words %>%
#     unlist()
#   
#   alpha.test <- word %in% alpha.words2
#   
#   if (alpha.test) {
#     trigram <- paste(bigram.pref, word, sep = "_")
#     trig.adj.freq <- trigram.table %>%
#       filter(feature == trigram) %$%
#       adj.freq
#     bi.freq <- bigram.table %>%
#       filter(feature == bigram.pref) %$%
#       frequency
#     qbo.value <- trig.adj.freq / bi.freq
#   } else {
#     alpha.value <- bigram.table %>%
#       filter(feature == bigram.pref) %$%
#       alpha.value
#     qbo.wv <- unigram.table %>%
#       filter(feature == word) %$%
#       qbos
#     beta.qbo.sums.wv <- bigram.table %>%
#       filter(feature == bigram.pref) %$%
#       beta.qbo.sums
#     qbo.value <- alpha.value * qbo.wv / beta.qbo.sums.wv
#   }
#   
#   return(qbo.value)
#   
# }
# 
# uni.qbos3 <- map_dbl(unigrams$feature, QboTrigram,
#                      preceding.words = bigram.prefix, unigram.table = unigrams,
#                      bigram.table = bigrams, trigram.table = trigrams)
# 
# unigrams <- unigrams %>%
#   mutate(qbos3 = uni.qbos3)
# rm(uni.qbos3)
# 
# unigrams %>%
#   select(feature, qbos3) %>%
#   arrange(desc(qbos3)) %>%
#   print()


unigrams <- corpus1 %>%
  tokens(n = 1) %>%
  dfm() %>%
  textstat_frequency() %>%
  select(feature, frequency) %>%
  mutate(n = 1) %>%
  ApplyWCAttribute() %>%
  ApplyQMLs()


# Should be called only once but still created inside a function that uses it
vocabulary.tokens <- tokens(unigrams$feature, n = 1)

# Move adjusted frequency to a function
bigrams <- corpus1 %>%
  tokens(n = 2) %>%
  dfm() %>%
  textstat_frequency() %>%
  select(feature, frequency) %>%
  mutate(n = 2) %>%
  mutate(adj.freq = frequency - d2)

alphas1 <- map(unigrams$feature, GetAlphaWords, n1gram.vector = bigrams$feature)
beta.vals1 <- map_dbl(alphas1, GetBetaValues, vocab.tokens = vocabulary.tokens,
                      unigram.table = unigrams, value.type = "qml")

unigrams <- unigrams %>%
  mutate(alpha.words = alphas1) %>%
  mutate(qml.sum = beta.vals1)

alpha.vals1 <- map_dbl(unigrams$feature, GetAlphaValues, ngram.table = unigrams,
                       n1gram.table = bigrams, discount = d2)

unigrams <- unigrams %>%
  mutate(alpha.value = alpha.vals1)

rm(alphas1, beta.vals1, alpha.vals1)

trigrams <- corpus1 %>%
  tokens(n = 3) %>%
  dfm() %>%
  textstat_frequency() %>%
  select(feature, frequency) %>%
  mutate(n = 3) %>%
  mutate(adj.freq = frequency - d3)

alphas2 <- map(bigrams$feature, GetAlphaWords, n1gram.vector = trigrams$feature)

bigrams <- bigrams %>%
  mutate(alpha.words = alphas2)

alpha.vals2 <- map_dbl(bigrams$feature, GetAlphaValues, ngram.table = bigrams,
                       n1gram.table = trigrams, discount = d3)

bigrams <- bigrams %>%
  mutate(alpha.value = alpha.vals2)

rm(alphas2, alpha.vals2)









