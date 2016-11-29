#corpus=text.extract
#len=3
#term="tweet"
kwic <- function(len,term,corpus,num) {
  len=len
  term=term
  corpus=corpus
  num=num
  extraction <- list()
  doc<-list()
  for (i in 1:num) { # documents number
    lda.list <- unlist(strsplit(corpus[[i]], "\\s+"))
    loc<- grep(term, lda.list,perl=TRUE)
    for (y in 1:length(loc)) { # one term indexes
      ext <-  lda.list[(loc[y]-len):(loc[y]+len)] 
      ext.collapse <- paste(ext,collapse=" ")
      ext.sub <-gsub(term,paste("<b>",term,"</b>",sep=""),ext.collapse)
      doc[[y]] <- ext.sub
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