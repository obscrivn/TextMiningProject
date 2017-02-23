# word frequency table
listTerms <- function(x) {
corpus <- Corpus(VectorSource(x))
tdm <- TermDocumentMatrix(corpus)
dtm <- DocumentTermMatrix(corpus)
terms.matrix <- as.matrix(tdm)
x <- strsplit(x, "\\s+")
terms.lda <- table(unlist(x))
terms.lda <- sort(terms.lda, decreasing = TRUE)
len <- length(terms.lda)
#terms.matrix<-as.matrix(terms.lda)
d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
d$word <- row.names(d)
agg_freq <- aggregate(frequency ~ word, data = d, sum)
d <- d[order(d$word),]#frequency, decreasing = T), ] 
corpus.lda <- as.list(d$word)
info=list(d=d,corpus.lda=corpus.lda,len=len,tdm=tdm,dtm=dtm)
return(info)
}

frequencyTable <- function(x) {
 # x=x
  text <- paste(x, sep=" ")
 docs <- Corpus(VectorSource(text))
 docs <- tm_map(docs, removePunctuation)  
 docs <- tm_map(docs, removeNumbers) 
 docs <- tm_map(docs, tolower)
 docs <- tm_map(docs, stripWhitespace)
 docs <- tm_map(docs, PlainTextDocument)
 tdm <- TermDocumentMatrix(docs)
 #corpus <- list()
# for (i in 1:length(docs)) {
  # doc <-docs[[i]]$content
#   doc <-docs[[1]]$content
#   text.punct <-rm_citation(doc)
 #  text.punct <-rm_citation(text.punct, pattern="@rm_citation3")
 #  text.punct <-rm_citation(text.punct, pattern="@rm_citation2")
 #  text.punct <-rm_round(text.punct)
 #  text.punct <-rm_curly(text.punct)
 #  text.punct <-rm_square(text.punct)
 #  text.split<-strsplit(text.punct, "References|references|REFERENCES")
   #text.split <- unlist(text.p)
  # text<-text.split[[1]][1]
  # corpus[[i]] <- text
  # corpus <- text.punct
# }
# corpus <-unlist(corpus)
# corpus.paste <-paste(corpus, sep=" ")
# corpus.list <- strsplit(corpus.paste, "\\s+")
 #corpus.list <- strsplit(x, "\\s+")
 x <- tolower(x)
 x <- gsub("[[:punct:]]","",x)
 corpus.list <- strsplit(x, "\\s+")
 terms.clean <- table(unlist(corpus.list))
 terms.clean <- sort(terms.clean, decreasing = TRUE)
 # len <- length(terms.lda)
 terms.matrix.clean<-as.matrix(terms.clean)
 d <- data.frame(frequency = sort(rowSums(terms.matrix.clean), decreasing = TRUE))
 d$word <- row.names(d)
 agg_freq <- aggregate(frequency ~ word, data = d, sum)
 #agg_freq[1:10,]
 agg_freq <- agg_freq[order(agg_freq$frequency,decreasing = T),]
 #d <- d[order(d$frequency,decreasing = T),]#frequency, decreasing = T), ] 
 relfreq <- agg_freq$frequency/sum(agg_freq$frequency)
 #relfreq <- d$frequency/sum(d$frequency)
 dataf <-data.frame(agg_freq,round(relfreq,3))
 #dataf[1:10,]
 dataf <-data.frame(d,relfreq)
 corpus.freq <- as.list(dataf$word)
 #corpus.paste <-paste(x, sep=" ")
 list(dataf=dataf,corpus.freq=corpus.freq,corpus=corpus,d=d)
}

