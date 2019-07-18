# Functions --------------------------------------------------------------------

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

# Should the vocab be fed in as tokens?
# How long does it take to convert in either direction?
# - In a test of 51000 tokens it took 1.27 seconds to convert a character
# vector into tokens. Using the same tokens object it took 0.09 seconds to
# convert it back into a character vector.








# Script -----------------------------------------------------------------------

corpus1 <- c("SOS buy the book EOS",
             "SOS buy the book EOS",
             "SOS buy the book EOS",
             "SOS buy the book EOS",
             "SOS sell the book EOS",
             "SOS buy the house EOS",
             "SOS buy the house EOS",
             "SOS paint the house EOS")

d2 <- 0.7
d3 <- 0.7

unigram.prefix <- "the"
bigram.prefix <- c("sell", "the")

unigrams <- corpus1 %>%
  tokens(n = 1) %>%
  dfm() %>%
  textstat_frequency() %>%
  select(feature, frequency) %>%
  mutate(n = 1)

word.count <- sum(unigrams$frequency)
word.list <- unigrams$feature

unigrams <- unigrams %>%
  mutate(qml = frequency / word.count)

bigrams <- corpus1 %>%
  tokens(n = 2) %>%
  dfm() %>%
  textstat_frequency() %>%
  select(feature, frequency) %>%
  mutate(n = 2) %>%
  mutate(adj.freq = frequency - d2)

alphas.1 <- map(unigrams$feature, GetAlphaWords, n1gram.vector = bigrams$feature)
betas.1 <- map(alphas.1, betas1, vocabulary = word.list)

unigrams <- unigrams %>%
  mutate(alpha.words = alphas.1) %>%
  mutate(beta.words = betas.1)
rm(alphas.1, betas.1)



beta.qsums <- map_dbl(unigrams$beta.words, beta.qsum, unigram.table = unigrams)

unigrams <- unigrams %>%
  mutate(beta.q.sums = beta.qsums)
rm(beta.qsums)

alphas1a <- function(unigram, bigram.tkns, discount) {
  pattern1 <- paste0(unigram, "_*")
  bigram.matches <- bigram.tkns %>%
    tokens_select(pattern = pattern1)
  bigram.test <- bigram.matches %>%
    as.character() %>%
    length()
  
  # Test if vector is empty, and replace with NA if TRUE
  if (bigram.test == 0) {
    alpha.sums <- NA
  } else {
    bigram.matches <- bigram.matches %>%
      dfm() %>%
      textstat_frequency() %$%
      frequency
    
    alpha.sums <- discount * length(bigram.matches) / sum(bigram.matches)
  }
  
  return(alpha.sums)
}

alpha.values <- map_dbl(unigrams$feature, alphas1a, bigram.tkns = bigrams,
                        discount = d2)

unigrams <- unigrams %>%
  mutate(alpha.value = alpha.values)



trigrams <- corpus1 %>%
  tokens(n = 3)


alphas.2 <- map(bigrams$feature, GetAlphaWords, trigrams$feature)
betas.2 <- map(alphas.2, betas1, vocabulary = word.list)

bigrams <- bigrams %>%
  mutate(alpha.words = alphas.2) %>%
  mutate(beta.words = betas.2)
rm(alphas.2, betas.2)

alphas2a <- function(bigram, trigram.tkns, discount) {
  pattern2 <- paste0(bigram, "_*")
  trigram.matches <- trigram.tkns %>%
    tokens_select(pattern = pattern2)
  trigram.test <- trigram.matches %>%
    as.character() %>%
    length()
  
  # Test if vector is empty, and replace with NA if TRUE
  if (trigram.test == 0) {
    alpha.sums <- NA
  } else {
    trigram.matches <- trigram.matches %>%
      dfm() %>%
      textstat_frequency() %$%
      frequency
    
    alpha.sums <- discount * length(trigram.matches) / sum(trigram.matches)
  }
  
  return(alpha.sums)
}

alpha.values2 <- map_dbl(bigrams$feature, alphas2a, trigrams, discount = d3)

bigrams <- bigrams %>%
  mutate(alpha.value = alpha.values2)
rm(alpha.values, alpha.values2)

trigrams <- trigrams %>%
  dfm() %>%
  textstat_frequency() %>%
  select(feature, frequency) %>%
  mutate(n = 3) %>%
  mutate(adj.freq = frequency - d3)

# This is the first step that requires knowledge of the preceding word
QboBigram <- function(word, preceding.word, unigram.table, bigram.table) {
  alpha.words1 <- unigram.table %>%
    filter(feature == preceding.word) %$%
    alpha.words %>%
    unlist()
  
  alpha.test <- word %in% alpha.words1
  
  if (alpha.test) {
    bigram <- paste(preceding.word, word, sep = "_")
    big.adj.freq <- bigram.table %>%
      filter(feature == bigram) %$%
      adj.freq
    uni.freq <- unigram.table %>%
      filter(feature == preceding.word) %$%
      frequency
    qbo.value <- big.adj.freq / uni.freq
  } else {
    alpha.value <- unigram.table %>%
      filter(feature == preceding.word) %$%
      alpha.value
    qml.w <- unigram.table %>%
      filter(feature == word) %$%
      qml
    beta.q.sums.w <- unigram.table %>%
      filter(feature == preceding.word) %$%
      beta.q.sums
    qbo.value <- alpha.value * qml.w / beta.q.sums.w
  }
  
  return(qbo.value)
  
}

uni.qbos <- map_dbl(unigrams$feature, QboBigram,
                    preceding.word = unigram.prefix, unigram.table = unigrams,
                    bigram.table = bigrams)

unigrams <- unigrams %>%
  mutate(qbos = uni.qbos)



beta.qbosums <- map_dbl(bigrams$beta.words, beta.qbosum,
                        unigram.table = unigrams)

bigrams <- bigrams %>%
  mutate(beta.qbo.sums = beta.qbosums)
rm(uni.qbos, beta.qbosums)

QboTrigram <- function(word, preceding.words, unigram.table, bigram.table,
                       trigram.table) {
  bigram.pref <- paste(preceding.words, collapse = "_")
  alpha.words2 <- bigram.table %>%
    filter(feature == bigram.pref) %$%
    alpha.words %>%
    unlist()
  
  alpha.test <- word %in% alpha.words2
  
  if (alpha.test) {
    trigram <- paste(bigram.pref, word, sep = "_")
    trig.adj.freq <- trigram.table %>%
      filter(feature == trigram) %$%
      adj.freq
    bi.freq <- bigram.table %>%
      filter(feature == bigram.pref) %$%
      frequency
    qbo.value <- trig.adj.freq / bi.freq
  } else {
    alpha.value <- bigram.table %>%
      filter(feature == bigram.pref) %$%
      alpha.value
    qbo.wv <- unigram.table %>%
      filter(feature == word) %$%
      qbos
    beta.qbo.sums.wv <- bigram.table %>%
      filter(feature == bigram.pref) %$%
      beta.qbo.sums
    qbo.value <- alpha.value * qbo.wv / beta.qbo.sums.wv
  }
  
  return(qbo.value)
  
}

uni.qbos3 <- map_dbl(unigrams$feature, QboTrigram,
                     preceding.words = bigram.prefix, unigram.table = unigrams,
                     bigram.table = bigrams, trigram.table = trigrams)

unigrams <- unigrams %>%
  mutate(qbos3 = uni.qbos3)
rm(uni.qbos3)

unigrams %>%
  select(feature, qbos3) %>%
  arrange(desc(qbos3)) %>%
  print()











