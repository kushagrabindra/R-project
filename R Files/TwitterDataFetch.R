library(httr)
library(twitteR)
setup_twitter_oauth('4T4HL518fn0AFR1giSLKisY5U', 'GCBxvj0W3b7NomDauIMfbjx6VFhr45IIjTnp0JrO6nQN5TymwK', '4567773974-roQtQdQwg5hRO6Th5sVfOlMg5YqDCa0oJD1yMZL', 'vsQLqzMqbGyDi0yvoiUqJPwhdjPk9r41bYO1Sf8yNWqnh')
tweets <- searchTwitter('#porn', n=50) 
tweets.df <- do.call(rbind, lapply(tweets, as.data.frame))
write.csv(tweets.df,file = "Minor13.csv",row.names=FALSE)
