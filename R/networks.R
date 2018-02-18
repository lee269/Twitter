#From https://rpubs.com/cosmopolitanvan/twitternetworks


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

alltweets <- twListToDF(searchTwitter("#FoodisGREAT", n=5000, lang=NULL,since=NULL, until=NULL,locale=NULL, geocode=NULL, sinceID=NULL, maxID=NULL,resultType=NULL, retryOnRateLimit=120))


sp = split(alltweets, alltweets$isRetweet)
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))
el = as.data.frame(cbind(sender = tolower(rt$sender), receiver = tolower(rt$screenName)))
el = count(el, sender, receiver) 
el[1:5,] #show the first 5 edges in the edgelist


rt_graph <- graph_from_data_frame(d=el, directed=T)

glay = layout.fruchterman.reingold(rt_graph) 
lay <- layout_nicely(rt_graph)
plot(rt_graph, layout = lay)

plot(rt_graph)


glay = layout.fruchterman.reingold(rt_graph)
par(bg="gray15", mar=c(1,1,1,1))
plot(rt_graph, layout=lay,
     vertex.color="gray25",
     vertex.size=(degree(rt_graph, mode = "in")), #sized by in-degree centrality
     vertex.label = NA,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=edge_attr(rt_graph)$n/10, #sized by edge weight
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
title("Retweet Network", cex.main=1, col.main="gray95")


glay = layout.fruchterman.reingold(rt_graph)
par(bg="gray15", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
     vertex.color="gray25",
     vertex.size=(degree(rt_graph, mode = "in")), #sized by in-degree centrality
     vertex.label.family="sans",
     vertex.shape="circle",  #can also try "square", "rectangle", etc. More in igraph manual
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=(degree(rt_graph, mode = "in"))/300, #sized by in-degree centrality
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=edge_attr(rt_graph)$n/10, #sized by edge weight
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
title("Retweet Network", cex.main=1, col.main="gray95")


wc <- cluster_walktrap(rt_graph)
members <- membership(wc)
d3_rt <- igraph_to_networkD3(rt_graph, group = members)

forceNetwork(Links = d3_rt$links, Nodes = d3_rt$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 12, zoom = TRUE)



#Mentions

orig = sp[['FALSE']]

mentioned = 
  lapply(orig$text, function(tx) {
    matches = gregexpr('@[^([:blank:]|[:punct:])]+', tx)[[1]]
    sapply(seq_along(matches), function(i) 
      substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
  })

mentionEL = 
  lapply(seq_along(orig$text), function(i) {
    if(mentioned[[i]] == '')  
      return(NULL)
    lapply(mentioned[[i]], function(m)
      c(sender = as.character(orig$screenName[i]), receiver = m)) %>%
      do.call(rbind, .) %>% as.data.frame()
  }) %>% 
  do.call(rbind, .) %>%
  count(tolower(sender), tolower(receiver))

mn_graph <- graph_from_data_frame(d=mentionEL, directed=T)
glay = layout.fruchterman.reingold(mn_graph) 
plot(mn_graph)

wc1 <- cluster_walktrap(mn_graph)
mnmembers <- membership(wc1)

d3_mn <- igraph_to_networkD3(mn_graph, group = mnmembers)

forceNetwork(Links = d3_mn$links, Nodes = d3_mn$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 12, zoom = TRUE)


