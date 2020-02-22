library(rvest)
library(XML)
library(magrittr)

aurl <- "https://www.snapdeal.com/product/apple-iphone-7-32gb-gold/626098478748/reviews?page=2&sortBy=HELPFUL"

snapdeal_reviews <- NULL

for(i in 1:8){
  murl <- read_html(as.character(paste(aurl, sep = as.character(i))))
  rev <- murl %>%
    html_nodes(".user-review p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews, rev)
}
getwd()
write.table(snapdeal_reviews, file = "snapdeal_reviews.csv")

data <- read.csv("snapdeal_reviews.csv", header = T)

data <- as.character(data$x)

library(tm)

x <- Corpus(VectorSource(data))

#data_cleansing

x1 <- tm_map(x, tolower)
x1 <- tm_map(x1, removePunctuation)

x1 <- tm_map(x1, removeNumbers)

x1 <- tm_map(x1, removeWords, stopwords("english"))

inspect(x1[4])

x1 <- tm_map(x1, stripWhitespace)

#TermDocumentMatrix

tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)

tdm[5:10, 1:10]

#barplot
w <- rowSums(tdm)

barplot(w)

library(wordcloud)
wordcloud(words = names(w))

library(wordcloud2)
w_0 <- data.frame(names(w), w)
wordcloud2(w_0, size = 0.5, shape = "circle")

letterCloud(w_0, word= "ABC", wordSize = 0.5)

#1) they mostly talking about the word amazing... it seems that the product is in a good way.
#2) they mostly talked about the positive words like outstanding, happy, nice, wow, satisfaction...
#3) somepeople talked about gift and wife... it shows that most of them are gifted to the wife