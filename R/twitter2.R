library(twitteR)
library(dplyr)
library(qdap)
library(networkD3)
library(igraph)
library(stringr) 
library(here)

twitterkeys <- readRDS(here("twitterkeys", "twitterkeys.rds"))

setup_twitter_oauth(consumer_key = twitterkeys$consumer_key, 
                    consumer_secret = twitterkeys$consumer_secret,
                    access_token = twitterkeys$access_token, 
                    access_secret = twitterkeys$access_secret)

# Get most recent tweets and convert to df 
stream <- userTimeline("DefraStats", n = 3200, includeRts = TRUE)
alltweets <- twListToDF(stream)


# Separate tweets from retweets
tweets_split = split(alltweets, alltweets$isRetweet)
tweets <- tweets_split[['FALSE']]
retweets = mutate(tweets_split[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))


usersmentioned = 
  lapply(tweets$text, function(tx) {
    matches = gregexpr('@[^([:blank:]|[:punct:])]+', tx)[[1]]
    sapply(seq_along(matches), function(i) 
      substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
  })

usersfreqs = 
  lapply(seq_along(tweets$text), function(i) {
    if(usersmentioned[[i]] == '')  
      return(NULL)
    lapply(usersmentioned[[i]], function(m)
      c(sender = as.character(tweets$screenName[i]), receiver = m)) %>%
      do.call(rbind, .) %>% as.data.frame()
  }) %>% 
  do.call(rbind, .) %>%
  count(tolower(sender), tolower(receiver))




hashtagsmentioned = 
  lapply(tweets$text, function(tx) {
    matches = gregexpr('#[^([:blank:]|[:punct:])]+', tx)[[1]]
    sapply(seq_along(matches), function(i) 
      substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
  })

hashtagfreqs = 
  lapply(seq_along(tweets$text), function(i) {
    if(hashtagsmentioned[[i]] == '')  
      return(NULL)
    lapply(hashtagsmentioned[[i]], function(m)
      c(sender = as.character(tweets$screenName[i]), receiver = m)) %>%
      do.call(rbind, .) %>% as.data.frame()
  }) %>% 
  do.call(rbind, .) %>%
  count(tolower(sender), tolower(receiver))
