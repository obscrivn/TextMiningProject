tokenize <- function(x,y,remove_urls,remove_references,remove_punctuation,
                     remove_html,lower_case,remove_numbers,exceptions){
  text.extraction <- vector()
  lda.format <- vector()  
  novel.list <- list()
  punct.list <-vector()
  novel.lda <- list()
  num<-length(y$name)
  file.names <-y$name
  for (i in 1:num) {
    data<- x[i]
    # data<- text.extract[1]
    text <- paste(data, collapse = " ")
    text.punct <- data
    
    if (remove_urls==TRUE) {
      text.punct <- rm_email(text.punct)
      text.punct <- rm_url(text.punct)
      text.punct <- rm_twitter_url(text.punct)
    } else {text.punct <- text.punct}
    
    if (remove_references==TRUE) {
      text.punct <-rm_citation(text.punct)
      text.punct <-rm_citation(text.punct, pattern="@rm_citation3")
      text.punct <-rm_citation(text.punct, pattern="@rm_citation2")
      text.punct <-rm_round(text.punct)
      text.punct <-rm_curly(text.punct)
      text.punct <-rm_square(text.punct)
      text.split<-strsplit(text.punct, "References|references|REFERENCES")
      #text.split <- unlist(text.p)
      text.punct<-text.split[[1]]
    } else {text.punct <- text.punct}
    
    if (remove_html==TRUE) {
      text.punct <- rm_bracket(text.punct)
    } else {text.punct <- text.punct}
    
    if (lower_case==TRUE) {
      text.punct <- tolower(text.punct)
    } else {text.punct <- text.punct}
    
    if (remove_numbers==TRUE) {
      text.punct<-  gsub('[[:digit:]]+', '', text.punct)
    } else {text.punct <- text.punct}
    
    if (remove_punctuation==TRUE) {
      if (exceptions=="Keep apostrophe") {
        text.punct <- gsub("-", " ", text.punct) 
        text.punct <- strip(text.punct, char.keep="~~",digit.remove = FALSE, apostrophe.remove = FALSE,
                            lower.case = FALSE)
      }
      else if (exceptions=="Keep hyphen") {
        text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                            lower.case = FALSE)
      }
      else if (exceptions=="Keep apostrophe and hyphen") {
        text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                            lower.case = FALSE)}
      else if (exceptions=="No exceptions") {
        text.punct <- gsub("[^[:alnum:] ]", "", text.punct) }#{text.punct <- gsub("[^[:alnum:] ]", "", text.punct)}
    }else {text.punct <- text.punct}
    
    text.punct <- gsub("\\s\\s+"," ",text.punct)
    text.punct <- str_c(text.punct)
    text.punct<- str_trim(text.punct)  
    text.split<-strsplit(text.punct, " ")
    text.split <- unlist(text.split)
    data.del <- gsub("[A-Za-z0-9]"," \\1", data)
    data.del.w <- paste(data.del, collapse = " ")
    data.no.punct<- gsub("([!¿?;,¡:]|(\\.+))", " \\1 ", data.del.w)
    data.no.punct <-  gsub("\\s+"," ",data.no.punct)
    file.name <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", file.names[i])#input$file.article.txt$name[i])   
    # text.split<-strsplit(text.punct, " ")
    # text.split <- unlist(text.split)
    text.freq <- table(text.split)
    text.relative <- 100*(text.freq/sum(text.freq))
    novel.list[[file.name]] <- text.relative
    punct.list[i] <-data.no.punct
    novel.lda[i] <-text.punct
    lda.format[i] <- text.punct
    text.extraction[i] <- text
  }
  list(data=data,novel.list=novel.list,text.extraction=text.extraction,punct.list=punct.list,novel.lda=novel.lda,lda.format=lda.format,data=data)
}

tokenizeRdf <- function(x,y,remove_urls,remove_references,remove_punctuation,
                     remove_html,lower_case,remove_numbers,exceptions){
  text.extraction <- vector()
  lda.format <- vector()  
  novel.list <- list()
  punct.list <-vector()
  novel.lda <- list()
  num<-length(y)
  file.names <-y
  for (i in 1:num) {
    data<- x[i]
    # data<- text.extract[1]
    text <- paste(data, collapse = " ")
    text.punct <- data
    
    if (remove_urls==TRUE) {
      text.punct <- rm_email(text.punct)
      text.punct <- rm_url(text.punct)
      text.punct <- rm_twitter_url(text.punct)
    } else {text.punct <- text.punct}
    
    if (remove_references==TRUE) {
      text.punct <-rm_citation(text.punct)
      text.punct <-rm_citation(text.punct, pattern="@rm_citation3")
      text.punct <-rm_citation(text.punct, pattern="@rm_citation2")
      text.punct <-rm_round(text.punct)
      text.punct <-rm_curly(text.punct)
      text.punct <-rm_square(text.punct)
      text.split<-strsplit(text.punct, "References|references|REFERENCES")
      #text.split <- unlist(text.p)
      text.punct<-text.split[[1]]
    } else {text.punct <- text.punct}
    
    if (remove_html==TRUE) {
      text.punct <- rm_bracket(text.punct)
    } else {text.punct <- text.punct}
    
    if (lower_case==TRUE) {
      text.punct <- tolower(text.punct)
    } else {text.punct <- text.punct}
    
    if (remove_numbers==TRUE) {
      text.punct<-  gsub('[[:digit:]]+', '', text.punct)
    } else {text.punct <- text.punct}
    
    if (remove_punctuation==TRUE) {
      if (exceptions=="Keep apostrophe") {
        text.punct <- gsub("-", " ", text.punct) 
        text.punct <- strip(text.punct, char.keep="~~",digit.remove = FALSE, apostrophe.remove = FALSE,
                            lower.case = FALSE)
      }
      else if (exceptions=="Keep hyphen") {
        text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                            lower.case = FALSE)
      }
      else if (exceptions=="Keep apostrophe and hyphen") {
        text.punct <- strip(text.punct, char.keep="-",digit.remove = FALSE, apostrophe.remove = FALSE,
                            lower.case = FALSE)}
      else if (exceptions=="No exceptions") {
        text.punct <- gsub("[^[:alnum:] ]", "", text.punct) }#{text.punct <- gsub("[^[:alnum:] ]", "", text.punct)}
    }else {text.punct <- text.punct}
    
    text.punct <- gsub("\\s\\s+"," ",text.punct)
    text.punct <- str_c(text.punct)
    text.punct<- str_trim(text.punct)  
    text.split<-strsplit(text.punct, " ")
    text.split <- unlist(text.split)
    data.del <- gsub("[A-Za-z0-9]"," \\1", data)
    data.del.w <- paste(data.del, collapse = " ")
    data.no.punct<- gsub("([!¿?;,¡:]|(\\.+))", " \\1 ", data.del.w)
    data.no.punct <-  gsub("\\s+"," ",data.no.punct)
    file.name <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", file.names[i])#input$file.article.txt$name[i])   
    # text.split<-strsplit(text.punct, " ")
    # text.split <- unlist(text.split)
    text.freq <- table(text.split)
    text.relative <- 100*(text.freq/sum(text.freq))
    novel.list[[file.name]] <- text.relative
    punct.list[i] <-data.no.punct
    novel.lda[i] <-text.punct
    lda.format[i] <- text.punct
    text.extraction[i] <- text
  }
  list(data=data,novel.list=novel.list,text.extraction=text.extraction,punct.list=punct.list,novel.lda=novel.lda,lda.format=lda.format,data=data)
}
