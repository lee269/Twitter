library(twitteR)
library(tidydescription)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(readr)
library(tokenizers)
library(here)

twitterkeys <- readRDS(here("twitterkeys", "twitterkeys.rds"))

setup_twitter_oauth(consumer_key = twitterkeys$consumer_key, 
                    consumer_secret = twitterkeys$consumer_secret,
                    access_token = twitterkeys$access_token, 
                    access_secret = twitterkeys$access_secret)


defrastats = getUser('DefraStats')

followers <- defrastats$getFollowers()
followersdf <- twListToDF(followers)

replace_reg2 <- "https:\\/\\/t.co\\/[A-Za-z0-9]+|https:\\/\\/[A-Za-z0-9\\.\\/]+|http:\\/\\/[A-Za-z0-9\\.\\/]+"
unnest_reg2 <- "([^A-Za-z_ \\d#@']|'(?![A-Za-z_\\d#@]))" 

test <- followersdf[1:10, ]

tidy_bios <- test %>% 
  unnest_tokens(bigram, description, token = "word") %>%
  anti_join(stop_words)

x <- tidy_bios %>% 
     count(word) %>% 
     arrange(desc(n))

wordcloud(words = x$word, freq = x$n, max.words = 100)

