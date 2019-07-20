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
                          value.type = c("qml", "qbo.bi")) {
  
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
ApplyQboValues <- function(ngram.table, preceding.words) {
  
  unigrams <- filter(ngram.table, n == 1)
  bigrams <- filter(ngram.table, n == 2)
  trigrams <- filter(ngram.table, n == 3)
  
  vocabulary.tokens <- tokens(unigrams$feature, n = 1)

  uni.qbo.bis <- map_dbl(unigrams$feature, GetQboBigram,
                      preceding.word = preceding.words[2],
                      unigram.table = unigrams, bigram.table = bigrams)
  
  unigrams <- unigrams %>%
    mutate(qbo.bi = uni.qbo.bis) %>%
    mutate(qbo.sum = NA_real_)

  bi.qbo.sums <- map_dbl(bigrams$alpha.words, GetBetaValues,
                         vocab.tokens = vocabulary.tokens,
                         unigram.table = unigrams, value.type = "qbo.bi")
  
  bigrams <- bigrams %>%
    mutate(qbo.bi = NA_real_) %>%
    mutate(qbo.sum = bi.qbo.sums) %>%
    mutate(qbo.tri = NA_real_)
  
  uni.qbo.tris <- map_dbl(unigrams$feature, GetQboTrigram,
                          preceding.words = preceding.words,
                          unigram.table = unigrams, bigram.table = bigrams,
                          trigram.table = trigrams)
  
  unigrams <- mutate(unigrams, qbo.tri = uni.qbo.tris)
  
  trigrams <- trigrams %>%
    mutate(qbo.bi = NA_real_) %>%
    mutate(qbo.sum = NA_real_) %>%
    mutate(qbo.tri = NA_real_)
  
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

# Build a single frequency table of 1:3 grams and perform calculations
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

# This is the first step that requires knowledge of the preceding word(s)
previous.words <- c("sell", "the")

ngrams <- ApplyQboValues(ngrams, preceding.words = previous.words)

# This is the "prediction" step
ngrams %>%
  filter(n == 1) %>%
  select(feature, qbo.tri) %>%
  arrange(desc(qbo.tri)) %>%
  print()




















