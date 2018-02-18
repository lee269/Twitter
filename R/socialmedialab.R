library(SocialMediaLab)
library(dplyr)
library(igraph)
library(here)

twitterkeys <- readRDS(here("twitterkeys", "twitterkeys.rds"))

x <- Authenticate(socialmedia = "twitter",
                  accessTokenSecret = twitterkeys$access_secret,
                  accessToken = twitterkeys$access_token,
                  apiSecret = twitterkeys$consumer_secret,
                  apiKey = twitterkeys$consumer_key) %>% 
  Collect(searchTerm = "DefraGovUK")

z <- x %>% 
  Create(type = "Actor")

z1 <- z %>% PopulateUserInfo()

plot(z)

plot(z2, vertex.shape="none", vertex.label=V(z)$name, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85", edge.arrow.size = .4)

l <- layout_in_circle(z)
plot(z, layout = l)

z2 <- simplify(z, remove.multiple = TRUE, remove.loops = TRUE) 
plot(z2)