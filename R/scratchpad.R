library(twitteR)
library(dplyr)
library(lubridate)
library(igraph)
library(networkD3)
library(here)

twitterkeys <- readRDS(here("twitterkeys", "twitterkeys.rds"))

setup_twitter_oauth(consumer_key = twitterkeys$consumer_key, 
                    consumer_secret = twitterkeys$consumer_secret,
                    access_token = twitterkeys$access_token, 
                    access_secret = twitterkeys$access_secret)


source(here("R", "friendsnet.R"))
dl <- friendsnet(twittername = "HatfieldForest", outputdir = "~/Twitter/")

user <- read_csv("~/Twitter/DefraStats.csv")

user <- user %>% 
        mutate(friend = "DefraStats") %>% 
        select(friend, follows = name)


folls <- read_csv("~/Twitter/DefraStats_friends.csv")
colnames(folls) <- c("friend", "follows")

all <- rbind(user, folls)

el <- count(all, friend, follows)

friend_graph <- graph_from_data_frame(d = el, directed = T)

plot(friend_graph)

lay <- layout_with_kk(friend_graph)
par(bg="white", mar=c(1,1,1,1))
plot(friend_graph, 
     layout = lay,
     vertex.color = "wheat3",
     vertex.size = 10,
     vertex.label.family = "sans",
     vertex.shape = "circle",
     vertex.label.color = "black",
     edge.arrow.size = 0.2,
     edge.arrow.width = 0.2)


par(bg="gray15", mar=c(1,1,1,1))
plot(friend_graph, layout=lay,
     vertex.color="gray25",
     # vertex.size=(degree(friend_graph, mode = "in")), #sized by in-degree centrality
     vertex.label.family="sans",
     vertex.shape="circle",  #can also try "square", "rectangle", etc. More in igraph manual
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     # vertex.label.cex=(degree(friend_graph, mode = "in"))/300, #sized by in-degree centrality
     edge.arrow.size=0.4,
     edge.arrow.width=0.1,
     edge.width=edge_attr(friend_graph)$n/10, #sized by edge weight
     edge.color="gray95")
title("Retweet Network", cex.main=1, col.main="gray95")



wc <- cluster_walktrap(friend_graph)
members <- membership(wc)
d3_rt <- igraph_to_networkD3(friend_graph, group = members)

forceNetwork(Links = d3_rt$links, Nodes = d3_rt$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 12, zoom = TRUE,
             opacity = 0.8)




colnames(z) <- c("x", "y")