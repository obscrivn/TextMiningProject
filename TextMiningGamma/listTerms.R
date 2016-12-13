
listTerms <- function(x) {
# x <- text.extract 
z<-paste(x,collapse=" ")
  
  text.punct <- gsub("[^[:alnum:] ]", "", z)
  data <- gsub("[0-9]+","", text.punct)
  data.space <- gsub("\\s+"," ",data)
  data.space <- str_c(data.space)
  data.space<- str_trim(data.space)  
  text.split<-strsplit(data.space, " ")  
  terms.clean <- table(unlist(text.split))
  terms.clean <- sort(terms.clean, decreasing = TRUE)
  terms.matrix.clean<-as.matrix(terms.clean)
  d.clean <- data.frame(frequency = sort(rowSums(terms.matrix.clean), decreasing = TRUE))
  d.clean$word <- row.names(d.clean)
  agg_freq <- aggregate(frequency ~ word, data = d.clean, sum)
  d.clean <- d.clean[order(d.clean$word),]
  
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
head(d.clean)
info=list(d=d,corpus.lda=corpus.lda,len=len,d.clean=d.clean)
return(info)
}

