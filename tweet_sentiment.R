####  Data Mining in R ###

setwd("C:/Users/RLanguage/Desktop/airline-twitter-sentiment")


require(dplyr)
require(stringr)
require(ggplot2)

require(wordcloud)

require(tm)
require(RColorBrewer)
require(SnowballC)
require(reshape2)
require(lubridate)
require(scales)
#install.packages("syuzhet")
require(syuzhet)
####

tweet <- read.csv("Tweets.csv", stringsAsFactors = FALSE)

## removing twitter handle
handle_rm <- str_replace_all(tweet$text, "@\\w+", "")
handle_rm <- gsub("http\\w+","", handle_rm) ## removing http file or email
## converting into corpus
w_corps <- Corpus(VectorSource(handle_rm))
w_corps <- tm_map(w_corps, removePunctuation)
w_corps <- tm_map(w_corps, content_transformer(tolower))
w_corps <- tm_map(w_corps, removeWords, stopwords("english"))
w_corps <- tm_map(w_corps, stripWhitespace)
w_corps <- tm_map(w_corps, removeNumbers)
w_corps <- tm_map(w_corps, stemDocument)
w_corps <- tm_map(w_corps, PlainTextDocument)
### building wordcloud
pal <- brewer.pal(6,"Dark2")

set.seed(123)
wordcloud(words = w_corps, scale=c(5,0.1), max.words=150, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

#### 
tdm <- TermDocumentMatrix(w_corps)
#tweet$text

m_sentiment <- get_nrc_sentiment(tweet$text)
head(m_sentiment)

tweet <- cbind(tweet, m_sentiment)
colnames(tweet)

f_sentiment <- data.frame(colSums(tweet[,c(16:25)]))
#f_sentiment
names(f_sentiment) <- "count"

f_sentiment <- cbind("sentiment" = rownames(f_sentiment), f_sentiment)
rownames(f_sentiment) <- NULL

ggplot(data = f_sentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

