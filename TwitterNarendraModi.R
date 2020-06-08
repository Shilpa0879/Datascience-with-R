#install.packages(c("devtools", "rjson","bit64","httr"))
library(devtools)

#install.packages("twitteR")
library("twitteR") 

#install.packages("ROAuth") 
library("ROAuth") 

#install.packages("base64enc")
library(base64enc) 

#install.packages("httpuv")
library(httpuv)
#https://apps.twitter.com/ 
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

registerTwitterOAuth(cred)

Tweets <- userTimeline('narendramodi', n = 1000)
TweetsDF <- twListToDF(Tweets)
write.csv(TweetsDF, "Tweets_sarf.csv")

###Search with a key word
word_tweets<- searchTwitter('corona', n=100, lang="en", resultType = "recent")
TweetsDF <- twListToDF(word_tweets)
