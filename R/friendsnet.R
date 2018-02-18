# From https://stackoverflow.com/questions/38571069/r-looping-through-list-of-follower-to-get-ego-network-using-twitter-package
# Amended to use dplyr as original code didnt work.
# This one sort of works
# friendsnet <- function(tuser) {
#   require(twitteR)
#   require(dplyr)
#   
#   # if rate limit is hit, wait for 15 minutes
#   limit <- getCurRateLimitInfo()[53,3]
#   print(paste("Look up limit", limit))
#   if (limit == 0) {
#     print("sleeping for fifteen minutes")
#     Sys.sleep(900)
#   }
#   
#   # Find user
#   tuser <- getUser(tuser)
#   print(tuser$screenName)
#   
#   # Empty dataframe
#   df <- NULL
#   print("empty data frame")
#   
#   # Get names of friends
#   f <- lookupUsers(tuser$getFriendIDs())
#   f2 <- twListToDF(f) %>% 
#         select(id, name = screenName)
#   # f.id <- sapply(f, id)
#   # f.name <- sapply(f, screenName)
#   # f2 <- as.data.frame(cbind(f.id, f.name))
#   print("list of friends")
#   print(head(f2))
#   
#   for (i in f2$name) {
#     
#     # if rate limit is hit, wait for 15 minutes
#     limit <- getCurRateLimitInfo()[53,3]
#     print(paste("Look up limit", limit))
#     if (limit == 0) {
#       print("sleeping for fifteen minutes")
#       Sys.sleep(900)
#     }
#     A <- getUser(i)
#     friends.object <- lookupUsers(A$getFriendIDs())
#     # Convert list into data frame
#     friends <- twListToDF(friends.object) %>% 
#                 select(id, friends.name = screenName)
#     
#     
#     # friends.id <- sapply(friends.object,id)
#     # friends.name <- sapply(friends.object, screenName)
#     # friends <- as.data.frame(cbind(friends.id, friends.name))
#     
#     for (j in f2$name) {
#       if (i != j) {
#         if ((j %in% friends$friends.name) == TRUE) {
#           print(paste(i, "follows", j))
#           df <- rbind(df, data.frame(i, j))
#         }
#       }
#       
#     }
#     
#   }
#   z <- list("followers" = f2, "others" = df)
#   return(z)
# }


# My go at a better function

friendsnet <- function(twittername, outputdir = "~") {
  require(twitteR)
  require(dplyr)
  require(lubridate)
  require(readr)
  
  # if rate limit is hit, wait for 15 minutes
  limit <- getCurRateLimitInfo("friends") %>% 
    filter(resource == "/friends/ids") %>% 
    mutate(now = ymd_hms(Sys.time()), timeleft = seconds(interval(ymd_hms(Sys.time()), ymd_hms(reset))))
  
  print(paste("Look up limit", limit$remaining))
  if (limit$remaining == 1) {
    print(paste("sleeping for", limit$timeleft[1], "seconds until", limit$reset[1] ))
    Sys.sleep(limit$timeleft[1] + 1)
  }
  
  # Find user
  tuser <- getUser(twittername)
  print(tuser$screenName)
  
  # Empty dataframe
  df <- NULL
  print("empty data frame")
  
  # Get names of friends
  f <- lookupUsers(tuser$getFriendIDs())
  friends <- twListToDF(f) %>% 
    select(id, name = screenName)
  rownames(friends) <- NULL
  # f.id <- sapply(f, id)
  # f.name <- sapply(f, screenName)
  # friends <- as.data.frame(cbind(f.id, f.name))
  print("list of friends")
  print(head(friends))
  write_csv(x = friends, path = paste(outputdir, twittername, ".csv", sep = ""), col_names = TRUE, append = FALSE)
  
  # Loop through friends
  for (i in friends$name) {
    
    # if rate limit is hit, wait for 15 minutes
    limit <- getCurRateLimitInfo("friends") %>% 
      filter(resource == "/friends/ids") %>% 
      mutate(now = ymd_hms(Sys.time()), timeleft = seconds(interval(ymd_hms(Sys.time()), ymd_hms(reset))))
    
    print(paste("Look up limit", limit$remaining))
    if (limit$remaining == 1) {
      print(paste("sleeping for", limit$timeleft[1], "seconds until", limit$reset[1] ))
      Sys.sleep(limit$timeleft[1]+1)
    }
    
    # get friends details
    A <- getUser(i)
    friends.object <- lookupUsers(A$getFriendIDs())
    # Convert list into data frame
    friendsfollow <- twListToDF(friends.object) %>% 
      select(id, friends.name = screenName)
    
    
    # friends.id <- sapply(friends.object,id)
    # friends.name <- sapply(friends.object, screenName)
    # friends <- as.data.frame(cbind(friends.id, friends.name))
    
    for (j in friends$name) {
      if (i != j) {
        if ((j %in% friendsfollow$friends.name) == TRUE) {
          print(paste(i, "follows", j))
          df <- rbind(df, data.frame(i, j))
        }
      }
      
    }
    df <- as.data.frame(df)
    write_csv(x = df, path = paste(outputdir, twittername, "_friends.csv", sep = ""), col_names = FALSE, append = TRUE)    
    df <- NULL
  }
  # colnames(df) <- c("friend", "follows")
  # z <- list("friends" = friends, "following" = df)
  # return(z)
}




