install.packages("rvest")
install.packages("XML")
install.packages("magrittr")
library(rvest)
library(XML)
library(magrittr)

aurl <- "https://www.amazon.com/Xiaomi-Redmi-Factory-Unlocked-Smartphone/product-reviews/B07Y8YWTFL/ref=cm_cr_arp_d_viewopt_sr?ie=UTF8&reviewerType=all_reviews&filterByStar=critical&pageNumber=1"

amazon_reviews <- NULL

for(i in 1:20){
  murl <- read_html(as.character(paste(aurl, i, sep = "=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews, rev)
}

setwd("F://")
write.table(amazon_reviews, "realmereviews.csv")
getwd()

data <- read.csv("realmereviews.csv", header = T)
str(data)
data <-as.character(data$x)
length(data)
library(tm)

x <- Corpus(VectorSource(data))

inspect(x[1])

#Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x[1])
 
x1 <- tm_map(x1, removePunctuation)

x1 <- tm_map(x1, removeNumbers)

x1 <- tm_map(x1, removeWords, stopwords("english"))

inspect(x1[100])

x1 <- tm_map(x1, stripWhitespace)

#TermDocumentMatrix

tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)

#barplot
w <- rowSums(tdm)
w_1 <- subset(w, w>=20)
barplot(w)
barplot(w_1)

install.packages("wordcloud")
library(wordcloud)

w_2 <- sort(rowSums(tdm), decreasing = T)

wordcloud(words = names(w_2), freq = w_2)

#It is negative and critical reviews of the realme x2 mobile
#More people talked about wifi and hot.... it may be hot while using that phone longer because longer is also talked by more people
#More people talked about samsung, it seems like they compare the mobile with samsung... it may be either positive or negative
#Some people talked about disappointment, I think it is a bad review about phone

install.packages("wordcloud2")
library(wordcloud2)

w_f <- data.frame(names(w_2),w_2)

wordcloud2(w_f)
letterCloud(w_f, word = "ABC")
