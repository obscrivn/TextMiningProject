listTerms <- function(x) {
  
#x<-paste(x,sep=" ")
x <- strsplit(x, "\\s+")

terms.lda <- table(unlist(x))
terms.lda <- sort(terms.lda, decreasing = TRUE)
len <- length(terms.lda)
terms.matrix<-as.matrix(terms.lda)
d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
d$word <- row.names(d)
agg_freq <- aggregate(frequency ~ word, data = d, sum)
d <- d[order(d$word),]#frequency, decreasing = T), ] 
corpus.lda <- as.list(d$word)
vector.lda <- as.vector(unlist(corpus.lda))
info=list(vector.lda=vector.lda,d=d,corpus.lda=corpus.lda,len=len)
return(info)
}