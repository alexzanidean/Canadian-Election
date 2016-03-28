library(tm)
library(wordcloud)
library(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")

tweets <-read.csv("MulcairMain.csv")
tweets1 <- tweets[sample(1:nrow(tweets),7000, replace=FALSE),]
tweets.text <- tweets1$text
tweets.text <- gsub("http+", "", tweets.text)
clean_text = clean.text(tweets.text)

tweet_corpus = Corpus(VectorSource(clean_text))
tweet_corpus <- tm_map(tweet_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                              mc.cores=1
                              )
tweet_corpus <- tm_map(tweet_corpus, content_transformer(tolower), mc.cores=1)
tweet_corpus <- tm_map(tweet_corpus, removePunctuation, mc.cores=1)
tweet_corpus <- tm_map(tweet_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
tweet_corpus <- tm_map(tweet_corpus, function(x)removeWords(x,stopwords("french")), mc.cores=1)
#tweet_corpus <- tm_map(tweet_corpus, stemDocument, mc.cores=1)

tweet_corpus <- tm_map(tweet_corpus, removeWords, c("e280a6", "<e2><80><a6>","httpste280a6", "httptce280a6", "httpse280a6", "via", "amp"))

myDTM = TermDocumentMatrix(tweet_corpus, control = list(minWordLength = 3))
m = as.matrix(myDTM)
v = sort(rowSums(m), decreasing = TRUE)

png("MulcairMain.png", width=6, height=4, units="in", res=300)
wordcloud(names(v),v ,min.freq=10,max.words=70, random.order=T, colors=pal2, rot.per=0)
dev.off()
