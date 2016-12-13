#corpus=text.extract
#len=3
#term="tweet"
kwic <- function(x,w,z,y) {
  len=as.numeric(x)
  term=w
  corpus=z
  num=as.numeric(y)
  extraction <- list()
  doc<-list()
  for (i in 1:num) { # documents number
    corpus.collapse<-paste(corpus,collapse=" ")
    lda.list <- unlist(strsplit(corpus.collapse[[i]], "\\s+"))
    loc<- grep(term, lda.list,perl=TRUE)
    for (k in 1:length(loc)) { # one term indexes
    #  ext <-  lda.list[(loc[y]-len):(loc[y]+len)] 
      ext <-  lda.list[(loc[k]-len):(loc[k]+len)] 
      ext.collapse <- paste(ext,collapse=" ")
      ext.sub <-gsub(term,paste("<b>",term,"</b>",sep=""),ext.collapse)
      doc[[k]] <- ext.sub
    }
    doc<-unlist(doc)
    extraction[[i]] <- doc
  }
  extraction<-unlist(extraction)
  return(extraction)
}


split_chunks <- function(x,term) {
  text=x
  term=term
  text.punct <- gsub("[^[:alnum:] ]", "", text)
  data <- gsub("[0-9]+","", text.punct)
  data.space <- gsub("\\s+"," ",data)
  data.space <- str_c(data.space)
  data.space<- str_trim(data.space)  
  text.split<-strsplit(data.space, " ")
  d=unlist(text.split)
#  chunk <- function(x, n) split(x, sort(rank(x) %% n))
  my.chunk <- function(x, n) split(x, sort(rep(letters[1:n], each=n, len=length(x))))
  n <- 5 # split in 5 chuncks
  y <- my.chunk(d,n)
    t1 <- table(y$a)
    s1 <- t1[(term)]
    t2 <- table(y$b)
    s2 <- t2[(term)]
    t3 <- table(y$c)
    s3 <- t3[(term)]
    t4 <- table(y$d)
    s4 <- t4[(term)]
    t5 <- table(y$e)
    s5 <- t5[(term)]
    doc <- rep("doc1",5)

   matr <- data.frame(rbind(s1,s2,s3,s4,s5))
   matr1<-cbind(doc,matr)

gg<-ggplot(matr1,aes(y=doc,x=row.names(matr1),fill=matr1[2]))
gg <- gg + geom_tile(color="white", size=0.1) + scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkblue",na.value = "white")
gg <- gg + coord_equal()
gg <- gg + labs(x=NULL, y=NULL, title=paste("Heatmap of Term: ",term))
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=7))
gg <- gg + theme(plot.title=element_text(hjust=0))

return(gg)
}

heatmap <- function(x) {
text <-x#$text.extract#window.one()$text.extract
#data.del <- gsub("[A-Za-z0-9]"," \\1", text) # only punctuation is left with space
#data.punct <-  gsub("\\s+","",data.del)
data.num <- gsub("[A-Za-z0-9]+","", text)
s.col <- gsub("[!?]",1,data.num) # red
s.col <- gsub("[\"\'\\(\\)-]",3,s.col) # green
s.col <- gsub("[,\\.:;]",2,s.col) # blue
s.col <- gsub("[^1-3]", "", s.col)
s.split <- strsplit(s.col,"")
matr1 <-matrix(s.split[[1]])
n<-length(matr1)
f <- factorize(n)
divide <-f[1]
if (min(f)<4) {
  divide <- f[2]
}
matr <- matrix(s.split[[1]],ncol=divide,byrow = FALSE)
class(matr) <- "numeric" 
myImagePlot(matr, title="Punctuation Analysis")#,zlim=c(1,3))
}

# num.documents <- length(text.extract)
# text.extract <- unlist(text.extract)
# #corpus.lda <-  window.one()$lda.format
# # num.documents <- length(window.one()$lda.format)
# n.docs <- num.documents#as.numeric(length(num.documents))
# 
# # corpus.lda <- removeWords(corpus.lda, remove.words.file)
# # corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
# empty.string <- lapply(text.extract, function(x) gsub(" +", " ", x))
# pdf.corpus <- lexicalize(empty.string, lower=TRUE)
# 
# # corpus <- Corpus(VectorSource(corpus.lda))
# #  corpus <- Corpus(VectorSource(novel.vector))
# # corpus <- tm_map(corpus,removeWords,remove.words.file)
# # newtext <-tm_map(corpus,removeWords,input$remove_words)
# 
# # pdf.corpus <- lexicalize(newtext, lower=TRUE)
# # pdf.corpus <- lexicalize(corpus.lda, lower=TRUE)
# # pdf.corpus$vocab <- wordStem(pdf.corpus$vocab, language = "english")
# K=3
# alphaK=0.02
# etaK=0.02
# iterK=500
# num.words=3
# wc <- word.counts(pdf.corpus$documents)
# cutoff.lower=0
# to.remove <- as.numeric(names(wc)[wc<=cutoff.lower])
# pdf.corpus$documents <- filter.words(pdf.corpus$documents , to.remove)
# get.terms <- function(x) {
#   index <- match(x, vocab)
#   index <- index[!is.na(index)]
#   rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
# }
# K <- as.numeric(input$num)
# alphaK <-as.numeric(input$alpha)
# etaK<-as.numeric(input$eta)
# num.words <- as.numeric(input$word)
# iterK <-as.numeric(input$iter)
# pdf.lda <-
#   lda.collapsed.gibbs.sampler(pdf.corpus$documents,K,pdf.corpus$vocab,iterK, alpha=alphaK, eta=etaK, compute.log.likelihood=TRUE)
# topics <- top.topic.words(pdf.lda$topics, num.words, by.score = T)
# docs <- top.topic.documents(pdf.lda$document_sums, num.documents)
# p_topic <- as.vector(pdf.lda$topic_sums / sum(pdf.lda$topic_sums))
# lda.coordinates <- mat.or.vec(n.docs,K)
# for (i in 1:n.docs){
#   for (j in 1:K){
#     lda.coordinates[i,j] <-
#       sum(pdf.lda$assignments[[i]]==(j-1))/length(pdf.lda$assignments[[i]])
#   }
# }
# distance <- lda.coordinates#LdaAnalysis()$lda.coordinates
# class(distance)
# d<-dist(distance)
# # euclidean distances between the rows
# fit <- cmdscale(distance)#,eig=TRUE,k=3)#, k=2) # k is the number of dim
# # fit # view results
# # plot solution 
# x <- fit$points[,1]
# y <- fit$points[,2]
# p <- plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
#           main="Metric  MDS",	type="n")
# text(x, y,  cex=.9)  

# n.topics=3
# novel.vector = text.extract
# corpus <- Corpus(VectorSource(novel.vector))
# # corpus <- tm_map(corpus,removeWords,remove.words.file)
# # corpus <-tm_map(corpus,removeWords,input$remove_words)
# tdm <-DocumentTermMatrix(corpus)   
# out <- readCorpus(tdm, type="dtm")
# documents <- out$documents
# head(documents)
# class (documents)
# vocab <- out$vocab
# n.topics <- as.numeric(input$num)
# stmmodel <- stm(documents, vocab, n.topics, verbose=FALSE)


