##### Install and Activate Packages
#install.packages("twitteR", "RCurl", "RJSONIO", "stringr")
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)

##### Declare Twitter API Credentials
api_key <- "UQ4KpQE1ij5HuPfXY1DCFUlS3" # From dev.twitter.com
api_secret <- "hPridFxTDfrlbSD8zBSbjHMT18sgemTAnW3wXFDcVV8LqrOtYr" # From dev.twitter.com
token <- "718428288377556992-5v4UYx1tG1R1enBvMvGrdtiiM67U9qK" # From dev.twitter.com
token_secret <- "ylJm4vuUyVKdsQAUbytqxBGMUwtvoJMTxbouy1fbsNtbJ" # From dev.twitter.com

##### Run Twitter Searchs. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).
tweets <- searchTwitter("#toscanadivino OR #ToscanaDivino OR 'toscanadivino' OR from:toscanadivino OR to:toscanadivino", n=1000)

##### Transform tweets list into a data frame
tweets.df <- twListToDF(tweets)
head(tweets.df)
summary(tweets.df)
tweets.df$text

##### Use the searchTwitter function to only get tweets within 50 miles of Los Angeles
#tweets_geolocated <- searchTwitter("Obamacare + ACA + 'Affordable Care Act' + #ACA", n=100, lang="en", geocode="34.049933,-118.240843,50mi", since="2014-08-20")
#tweets_geolocated.df <- twListToDF(tweets_geolocated)
