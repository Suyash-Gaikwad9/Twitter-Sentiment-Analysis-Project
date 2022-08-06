#read dataset
apple = read.csv(file.choose(),header=T)

str(apple)
View(apple)
dim(apple)
colnames(apple)


#build corpus
library(tm)
corpus <- iconv(apple$text,to='UTF-8', sub = "byte")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5]) #inspect 1st 5 tweets

# Clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

#remove punctuation marks
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

#remove numbers
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

#remove common english words that don't add enough value
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

#remove URL's
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])


#remove extra space left after cleaning
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')


cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])




#term document matrix : essentially structurize the data
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)#convert it into matrix
tdm[1:10, 1:20]


# Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=25)#when the word frequency is more than 25 only those words are included in w
barplot(w,
        las = 2,
        col = rainbow(50))


# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(7, 0.3),
          rot.per = 0.7)



library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.9,
           shape = 'star',
           rotateRatio = 0.5,
           minSize = 1)
#letterCloud(w,
#           word = "A",
#            size=1)



# Sentiment analysis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


# Read file
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to='UTF-8', sub = "byte")


# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')#testing the sentiment function for accuracy
# Bar plot


barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets Before Earnings Report')



#After earnings report
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to='UTF-8', sub = "byte")


# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')#testing the sentiment function for accuracy


# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets After Earnings Report')



