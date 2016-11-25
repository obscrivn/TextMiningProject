
listTerms <- function(x) {
 #x <- text.extract 
#x<-paste(x,sep=" ")
 y <- strsplit(corpus.paste, "\\s+")
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
info=list(d=d,corpus.lda=corpus.lda,len=len)
return(info)
}
#listTerms(text.extract)
