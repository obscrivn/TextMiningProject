ExtractContent <- function (x){
  num <-length(x$name)
  textum <- vector()
  for (i in 1:num) {
    uris.name <- x$datapath[i]
    text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = FALSE)  
    data=enc2utf8(text.scan)
    text <- paste(data, collapse = " ")
    text <-gsub("-\\s+", "", text) 
    textum[i] <- text
  } 
  return(textum)
}