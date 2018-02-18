library(tidytext)
library(dplyr)
library(tm)
library(topicmodels)
library(SnowballC)

followers <- read.csv("dsfollowers.csv", header = TRUE)

orig_text <- followers %>%
              select(text = description) %>% 
              mutate(docno = row_number())


orig_text$text <- as.character(orig_text$text)

data("stop_words")


cleaned_text <- orig_text %>%
  mutate(text = gsub("[^[:alnum:]///' ]", "", text)) %>% 
  mutate(text = removePunctuation(text)) %>%
  mutate(text = removeNumbers(text)) %>%
  mutate(text = tolower(text)) %>%
  mutate(text = removeWords(text, as.vector(stop_words$word))) %>%
  mutate(text = stripWhitespace(text))


#extract ngrams from raw data
terms <- cleaned_text %>%
  unnest_tokens(output = ngram, input = text) 

# calc ngram frequency per comment(document)
tidy_terms <- terms %>%
  group_by(docno, ngram) %>%
  summarise(count = n()) %>% 
  ungroup()

# stem terms
cleaned_terms <- tidy_terms %>%
  mutate(ngram = wordStem(ngram)) %>% 
  ungroup()

# Make DTM
dtm <- cast_dtm(data = cleaned_terms, document = docno, term = ngram, value = count)


