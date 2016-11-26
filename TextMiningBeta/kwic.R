#corpus=text.extract
#len=3
#term="paper"
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
   # doc.collapse <-paste(doc, collapse=" ")
    #ext <-  lda.list[(loc-len):(loc+len)]   # EXTRACTING SECTIONS 
    # ext.collapse <- paste(ext,collapse=" ")
    extraction[[i]] <- doc#.collapse
  }
  extraction<-unlist(extraction)
  return(extraction)
}




