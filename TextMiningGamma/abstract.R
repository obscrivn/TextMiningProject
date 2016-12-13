extractAbstract <- function(x,y) {
  extraction <- vector()
  doc<-vector()
    corpus.lda <- x
    num <-length(y$name)
    for (i in 1:num) {
      lda.list <- unlist(strsplit(corpus.lda[[i]], "\\s+"))
      loc.a<- grep("Abstract|ABSTRACT", lda.list)
      loc.b <- grep("Introduction|INTRODUCTION",lda.list)#grep("Introduction|INTRODUCTION",x)
      doc <-  lda.list[(loc.a+1):(loc.b-1)]
      doc.collapse <- paste(doc,collapse=" ")
      extraction[i] <- doc.collapse
      # lines =  x[(loc.a[1]+1):(loc.b-1)]   # EXTRACTING SECTIONS   
    }
return(extraction)
}