listTerms <- function(x) {
 #x <- text.extract 
#x<-paste(x,sep=" ")
  doc.vect <- VectorSource(x)
  docs <-Corpus(doc.vect)
  docs <- tm_map(docs, removePunctuation)  
  docs <- tm_map(docs, removeNumbers) 
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)
  tdm <- TermDocumentMatrix(docs)
  corpus <- list()
  for (i in 1:length(docs)) {
    doc <-docs[[1]]$content
    corpus[[i]] <- doc
  }
  corpus <-unlist(corpus)
  corpus.paste <-paste(corpus, sep=" ")
  corpus.list <- strsplit(corpus.paste, "\\s+")
  terms.clean <- table(unlist(corpus.list))
  terms.clean <- sort(terms.clean, decreasing = TRUE)
 # len <- length(terms.lda)
  terms.matrix.clean<-as.matrix(terms.clean)
  d.clean <- data.frame(frequency = sort(rowSums(terms.matrix.clean), decreasing = TRUE))
  d.clean$word <- row.names(d.clean)
  agg_freq <- aggregate(frequency ~ word, data = d.clean, sum)
  d.clean <- d.clean[order(d.clean$frequency,decreasing = T),]#frequency, decreasing = T), ] 
  relfreq <- d.clean$frequency/sum(d.clean$frequency)
  dataf <-data.frame(d.clean,relfreq)
  corpus.clean <- as.list(d.clean$word)
 y <- strsplit(x, "\\s+")
terms.lda <- table(unlist(y))
terms.lda <- sort(terms.lda, decreasing = TRUE)
len <- length(terms.lda)
terms.matrix<-as.matrix(terms.lda)
d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
d$word <- row.names(d)
agg_freq <- aggregate(frequency ~ word, data = d, sum)
d <- d[order(d$word),]#frequency, decreasing = T), ] 
corpus.lda <- as.list(d$word)
#vector.lda <- as.vector(unlist(corpus.lda))
info=list(d=d,corpus.lda=corpus.lda,corpus=corpus,len=len,d.clean=d.clean,tdm=tdm)
return(info)
}
#listTerms(text.extract)
