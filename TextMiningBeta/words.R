# word frequency table
listTerms <- function(x) {
#x<-paste(x,sep=" ")
corpus <- Corpus(VectorSource(x))
tdm <- TermDocumentMatrix(corpus)
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
#vector.lda <- as.vector(unlist(corpus.lda))
info=list(d=d,corpus.lda=corpus.lda,len=len,tdm=tdm)
return(info)
}

# mycorpus <- extraction
# doc.vect <- VectorSource(mycorpus)
# corpus.tm <-Corpus(doc.vect)
# corpus.tm <- tm_map(corpus.tm,removeWords,c("tweet","Twitter"))
# corpus.tm[[1]]
# length(corpus.tm)
# corpus.lda <- list()
# for (i in 1:length(corpus.tm)) {
#   doc <-corpus.tm[[i]]$content
#   corpus.lda[[i]] <- doc
# }
# corpus.lda <-unlist(corpus.lda)
# #corpus <- removeWords(mycorpus, c(stopWordsTxt()))
# corpus.paste <-paste(corpus.lda, sep=" ")
# corpus.list <- strsplit(corpus.paste, "\\s+")
# terms <- table(unlist(corpus.list))
# terms.sorted <- sort(terms, decreasing = TRUE)
