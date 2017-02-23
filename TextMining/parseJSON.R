#Parsing JSON from Google Books API
parseJSON <-function(x) {
  #Extracting metadata
  titles <- x$items$volumeInfo$title
  num <-length(titles)
  subtitles_temp <- x$items$volumeInfo$subtitle
  subtitles <- unlist(lapply(subtitles_temp, function(x) { x[is.na(x)] <- "" ; x }))
  authors_temp <- x$items$volumeInfo$author
  authors <- sapply(authors_temp,function(x) paste(unlist(x),collapse=", "))
  dates <-x$items$volumeInfo$publishedDate
  dates_temp <- strsplit(as.character(dates), "-")
  dates <- sapply(dates_temp, "[[", 1)
  #  metadataJSON<-cbind(title = titles, author = authors, date = dates)
  description_temp <- x$items$volumeInfo$description
  descriptions <- unlist(lapply(description_temp, function(x) { x[is.na(x)] <- "" ; x }))
  textum <- vector()
  # for (i in 1:num) {
  # 
  #   text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = FALSE)  
  #   data=enc2utf8(text.scan)
  #   text <- paste(data, collapse = " ")
  #   text <-gsub("-\\s+", "", text) 
  #   textum[i] <- text
  # } 
  corpus <- paste(titles, subtitles, descriptions, sep = " ")
  
  return(list(titles = titles, authors = authors, dates = dates, corpus = corpus))  
}
