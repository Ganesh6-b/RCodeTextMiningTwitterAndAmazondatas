install.packages("twitteR")

install.packages("ROAuth")
library(twitteR)
library(ROAuth)

cred <- OAuthFactory$new(
  consumerKey = "dFyfWLiaDVUdJpe1q1PWvpcN8", #my api key
  consumerSecret = "4FxQrdtHkLRMKmNBbI4c8DwyBTzBXklwQA88HTZxniftBIs9C9",
  requestURL = "https://api.twitter.com/oauth/request token",
  accessURL = "https://api.twitter.com/oauth/access token",  
  authURL = "https://api.twitter.com.oauth.authorize")
save(cred, file = "twitter authentication.Rdata")
getwd()

load("twitter authentication.Rdata")

install.packages("base64enc")
library(base64enc)

library(httpuv)

setup_twitter_oauth("dFyfWLiaDVUdJpe1q1PWvpcN8", #consumer key
                    "4FxQrdtHkLRMKmNBbI4c8DwyBTzBXklwQA88HTZxniftBIs9C9", #consumer secret
                    "806470133359067137-52omycwup1h6x99IbrBudWAZIXvwxCA", #Access token
                    "WBfeo3Rt1g6SgJxKoizjTYLydUmBmpvF9AV3TzlI6u27I") #Access token secret

tweets <- userTimeline("imVkohli", n = 1000, includeRts = T)

tweetsdf <- twListToDF(tweets)
setwd("F://")
library(openxlsx)
write.xlsx(tweetsdf, "kohlitweets.xlsx")

data <- read.xlsx("kohlitweets.xlsx",1)

data <- data$text
data
x <- as.character(data)
class(x)

library(tm)
x <- Corpus(VectorSource(x))

#dataClensing

x1 <- tm_map(x, tolower)
x1 <- tm_map(x1, removePunctuation)
inspect(x1[5])
x1 <- tm_map(x1, removeNumbers)
x1 <- tm_map(x1, removeWords, stopwords("en"))

#remove URL
removeURL <- function(z) gsub("http[[:alnum:]]*", '', z)
x1 <- tm_map(x1, content_transformer(removeURL))
inspect(x1[5])

x1 <- tm_map(x1, stripWhitespace)

#TDM

tdm <- TermDocumentMatrix(x1)

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]

#barplot

w <- rowSums(tdm)

w_sub <- subset(w, w >= 5)
barplot(w_sub, las = 2, col = rainbow(20))

library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

library(wordcloud2)

w_0 <- data.frame(names(w_sub), w_sub)
wordcloud2(w_0)

#1) he is mostly talking about anushkasharma . she is his wife. Hence, he made a more concentration on his wife
#2) he mostly talked about thank and thanks. hence he proofs that he response(respect) to the persons who helped him or with him.
#3) He always use a word Jai hind. it shows that nations affection 
#4) He mostly talked about that daily played matches.. like using good, bad, day, guys, boys etc.,
#5) Expressing his feelings through twitter based on the match they performed