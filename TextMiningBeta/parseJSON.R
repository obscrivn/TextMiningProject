#Parsing JSON from Google Books API
parseJSON <-function(x) {
  #Extracting metadata
  titles <- x$items$volumeInfo$title
  authors_temp <- x$items$volumeInfo$author
  authors <- sapply(authors_temp,function(x) paste(unlist(x),collapse=", "))
  dates <-x$items$volumeInfo$publishedDate
  dates_temp <- strsplit(as.character(dates), "-")
  dates <- sapply(dates_temp, "[[", 1)
  #  metadataJSON<-cbind(title = titles, author = authors, date = dates)
  
  #Extracting text
  description <- x$items$volumeInfo$description
  #  corpus <- paste(description, collapse = " ")
  corpus <- gsub("-\\s+", "", description) 
  return(list(titles = titles, authors = authors, dates = dates, corpus = corpus))  
}
