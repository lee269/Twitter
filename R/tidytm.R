library(twitteR)
library(tidytext)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(readr)
library(here)

twitterkeys <- readRDS(here("twitterkeys", "twitterkeys.rds"))

setup_twitter_oauth(consumer_key = twitterkeys$consumer_key, 
                    consumer_secret = twitterkeys$consumer_secret,
                    access_token = twitterkeys$access_token, 
                    access_secret = twitterkeys$access_secret)


tweetslist <- userTimeline("DefraGovUK", n = 3200, includeRts = TRUE)
tweetsdf <- twListToDF(tweetslist)


# http://tidytextmining.com/twitter.html#favorites-and-retweets

tweets <- tweetsdf %>% 
          mutate(created_at = ymd_hms(created))

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
# attempt to get rid of punctuation, t.co.links etc
# play at https://regexr.com
replace_reg2 <- "https:\\/\\/t.co\\/[A-Za-z0-9]+|https:\\/\\/[A-Za-z0-9\\.\\/]+|http:\\/\\/[A-Za-z0-9\\.\\/]+"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
#adds extra space so as to not delete them 
unnest_reg2 <- "([^A-Za-z_ \\d#@']|'(?![A-Za-z_\\d#@]))" 

# tidy_tweets <- tweets %>% 
#   filter(!str_detect(text, "^(RT|@)")) %>%
#   mutate(text = str_replace_all(text, replace_reg, "")) %>%
#   unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
#   anti_join(stop_words)
# In website example the unnest_token with regex did not work
# hopefully this has the same effect, eg removing all but text and hashtags


tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_replace_all(text, replace_reg2, "")) %>%
  mutate(text = str_replace_all(text, unnest_reg2, "")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


totals <- tidy_tweets %>% 
  group_by(id) %>% 
  summarise(rts = sum(retweetCount)) 


word_by_rts <- tidy_tweets %>% 
  group_by(id, word) %>% 
  summarise(rts = first(retweetCount)) %>% 
  group_by(word) %>% 
  summarise(retweets = median(rts), uses = n()) %>%
  filter(retweets != 0) %>%
  ungroup()

word_by_rts %>% 
  filter(uses >= 5) %>%
  arrange(desc(retweets))


word_by_rts %>%
  filter(uses >= 5) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweets)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of retweets for tweets containing each word")

totals <- tidy_tweets %>% 
  group_by(id) %>% 
  summarise(rts = sum(favoriteCount)) 


word_by_favs <- tidy_tweets %>% 
  group_by(id, word) %>% 
  summarise(favs = first(favoriteCount)) %>% 
  group_by(word) %>% 
  summarise(favorites = median(favs), uses = n()) %>%
  filter(favorites != 0) %>%
  ungroup()

word_by_favs %>%
  filter(uses >= 5) %>%
  top_n(10, favorites) %>%
  arrange(favorites) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, favorites)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of favorites for tweets containing each word")


x <- tidy_tweets %>% 
     count(word) %>% 
     arrange(desc(n))


custom_stop_words <- bind_rows(data_frame(word = c("amp"), 
                                          lexicon = c("custom")), 
                               stop_words)

x <- x %>% anti_join(custom_stop_words)

wordcloud(words = x$word, freq = x$n, max.words = 100, random.order = FALSE)

defrastats <- read_csv("~/defrastats.csv")

x <- defrastats$screenName
z <- lookupUsers(x)
z1 <- twListToDF(z)